import unittest
import jupyter_kernel_test as jkt

from jupyter_client.manager import start_new_kernel


without_rich_display = '''\
options(jupyter.rich_display = FALSE)
{}
options(jupyter.rich_display = TRUE)
'''
#this will not work!
#withr::with_options(list(jupyter.rich_display = FALSE), {})


class InstallspecTests(jkt.KernelTests):
    # just a small test, it's the same kernel after all...
    kernel_name = 'testir'

    language_name = 'R'

    code_hello_world = 'print("hello, world")'


class IRkernelTests(jkt.KernelTests):
    kernel_name = 'ir'

    language_name = 'R'

    def _execute_code(self, code, tests=True):
        self.flush_channels()

        reply, output_msgs = self.execute_helper(code)

        self.assertEqual(reply['content']['status'], 'ok', '{0}: {0}'.format(reply['content'].get('ename'), reply['content'].get('evalue')))
        if tests:
            self.assertGreaterEqual(len(output_msgs), 1)
            # the irkernel only sends display_data, not execute_results
            self.assertEqual(output_msgs[0]['msg_type'], 'display_data')
        return reply, output_msgs

    code_hello_world = 'print("hello, world")'

    completion_samples = [
        {
            'text': 'zi',
            'matches': {'zip'},
        },
    ]

    complete_code_samples = ['1', 'print("hello, world")', 'f <- function(x) {\n  x*2\n}']

    incomplete_code_samples = ['print("hello', 'f <- function(x) {\n  x*2']

    code_display_data = [
        {'code': '"a"', 'mime': 'text/plain'},
        {'code': '"a"', 'mime': 'text/html'},
        {'code': '"a"', 'mime': 'text/latex'},
        {'code': '1:3', 'mime': 'text/plain'},
        {'code': '1:3', 'mime': 'text/html'},
        {'code': '1:3', 'mime': 'text/latex'},
        {'code': without_rich_display.format('"a"'), 'mime': 'text/plain'},
        {'code': without_rich_display.format('1:3'), 'mime': 'text/plain'},
    ]

    def test_display_vector(self):
        """display of vectors"""
        code = '1:3'
        reply, output_msgs = self._execute_code(code)
        
        # we currently send those formats: text/plain, text/html, text/latex, and text/markdown
        data = output_msgs[0]['content']['data']
        self.assertEqual(len(data), 4, data.keys())
        self.assertEqual(data['text/plain'], '[1] 1 2 3')
        self.assertIn('text/html', data)
        
        # this should not be a test of the repr functionality...
        self.assertIn('</li>', output_msgs[0]['content']['data']['text/html'])
        self.assertIn('text/latex', output_msgs[0]['content']['data'])
        self.assertIn('text/markdown', output_msgs[0]['content']['data'])

    def test_display_vector_only_plaintext(self):
        """display of plain text vectors"""
        code = without_rich_display.format('1:3')
        reply, output_msgs = self._execute_code(code)
        data = output_msgs[0]['content']['data']
        self.assertEqual(len(data), 1, data.keys())
        self.assertEqual(data['text/plain'], '[1] 1 2 3')

    def test_irkernel_plots(self):
        """plotting"""
        code = 'plot(1:3)'
        reply, output_msgs = self._execute_code(code)
        
        # we currently send three formats: png, svg and text/plain
        data = output_msgs[0]['content']['data']
        self.assertEqual(len(data), 3, data.keys())
        self.assertEqual(data['text/plain'], 'plot without title')
        self.assertIn('image/svg+xml', data)
        self.assertIn('image/png', data)
        
        # we isolate only svg plots
        metadata = output_msgs[0]['content']['metadata']
        self.assertEqual(len(metadata), 1, metadata.keys())
        self.assertEqual(len(metadata['image/svg+xml']), 1)
        self.assertEqual(metadata['image/svg+xml']['isolated'], True)

    def test_irkernel_plots_only_PNG(self):
        """plotting PNG"""
        # the reset needs to happen in another execute because plots are sent after either
        # the next plot is opened or everything is executed, not at the time when plot
        # command is actually happening.
        # (we have a similar problem with width/... but we worked around it by setting the
        # appropriate options to the recorderdplot object)
        code = '''\
            old_options <- options(jupyter.plot_mimetypes = c('image/png'))
            plot(1:3)
        '''
        reply, output_msgs = self._execute_code(code)
        
        # Only png, no svg or plain/text
        data = output_msgs[0]['content']['data']
        self.assertEqual(len(data), 1, data.keys())
        self.assertIn('image/png', data)
        
        # And reset
        code = 'options(old_options)'
        reply, output_msgs = self._execute_code(code, tests=False)

    def test_irkernel_df_default_rich_output(self):
        """data.frame rich representation"""
        code = 'data.frame(x = 1:3)'
        reply, output_msgs = self._execute_code(code)
        
        # we currently send three formats: text/plain, html, and latex
        data = output_msgs[0]['content']['data']
        self.assertEqual(len(data), 3, data.keys())

    def test_irkernel_df_no_rich_output(self):
        """data.frame plain representation"""
        code = '''
            options(jupyter.rich_display = FALSE)
            data.frame(x = 1:3)
            options(jupyter.rich_display = TRUE)
        '''
        reply, output_msgs = self._execute_code(code)
        
        # only text/plain
        data = output_msgs[0]['content']['data']
        self.assertEqual(len(data), 1, data.keys())

    def test_html_isolated(self):
        """HTML isolation"""
        code = '''
            repr_html.full_page <- function(obj) sprintf('<html><body>%s</body></html>', obj)
            structure(0, class = 'full_page')
        '''
        reply, output_msgs = self._execute_code(code)
        
        data = output_msgs[0]['content']['data']
        self.assertEqual(len(data), 2, data.keys())
        self.assertEqual(data['text/html'], '<html><body>0</body></html>')
        
        metadata = output_msgs[0]['content']['metadata']
        self.assertEqual(len(metadata), 1, metadata.keys())
        self.assertEqual(len(metadata['text/html']), 1, metadata['text/html'].keys())
        self.assertEqual(metadata['text/html']['isolated'], True)

    def test_in_kernel_set(self):
        """jupyter.in_kernel option"""
        reply, output_msgs = self._execute_code('getOption("jupyter.in_kernel")')
        data = output_msgs[0]['content']['data']
        self.assertGreaterEqual(len(data), 1, data.keys())
        self.assertEqual(data['text/plain'], '[1] TRUE', data.keys())


if __name__ == '__main__':
    unittest.main()
