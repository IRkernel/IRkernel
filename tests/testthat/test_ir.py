import sys
from pathlib import Path

HERE = Path(__file__).parent
sys.path.insert(0, str(HERE / 'jkt'))

import unittest
import jupyter_kernel_test as jkt

from jupyter_client.manager import start_new_kernel
from jupyter_kernel_test.messagespec import validate_message


without_rich_display = '''\
options(jupyter.rich_display = FALSE)
{}
options(jupyter.rich_display = TRUE)
'''
#this will not work!
#withr::with_options(list(jupyter.rich_display = FALSE), {})

TIMEOUT = 15


class IRkernelTests(jkt.KernelTests):
    kernel_name = 'testir'

    language_name = 'R'

    def _execute_code(self, code, tests=True, silent=False, store_history=True):
        self.flush_channels()

        reply, output_msgs = self.execute_helper(code, silent=silent, store_history=store_history)

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
        
        # we currently send two formats: png, and text/plain
        data = output_msgs[0]['content']['data']
        self.assertEqual(len(data), 2, data.keys())
        self.assertEqual(data['text/plain'], 'plot without title')
        self.assertIn('image/png', data)
        
        # we isolate only svg plots, which are not included in the default types
        metadata = output_msgs[0]['content']['metadata']
        self.assertEqual(len(metadata), 0, metadata.keys())

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

        # nothing in metadata
        metadata = output_msgs[0]['content']['metadata']
        self.assertEqual(len(metadata), 0, metadata.keys())

        # And reset
        code = 'options(old_options)'
        reply, output_msgs = self._execute_code(code, tests=False)

    def test_irkernel_plots_only_SVG(self):
        # again the reset dance (see PNG)
        code = '''\
            old_options <- options(jupyter.plot_mimetypes = c('image/svg+xml'))
            plot(1:3)
        '''
        reply, output_msgs = self._execute_code(code)

        # Only svg, no png or plain/text
        data = output_msgs[0]['content']['data']
        self.assertEqual(len(data), 1, data.keys())
        self.assertIn('image/svg+xml', data)
        
        # svg output is currently isolated
        metadata = output_msgs[0]['content']['metadata']
        self.assertEqual(len(metadata), 1, metadata.keys())
        self.assertEqual(len(metadata['image/svg+xml']), 1)
        self.assertEqual(metadata['image/svg+xml']['isolated'], True)

        # And reset
        code = 'options(old_options)'
        reply, output_msgs = self._execute_code(code, tests=False)

    def test_irkernel_plots_without_rich_display(self):
        code = '''\
            options(jupyter.rich_display = FALSE)
            plot(1:3)
        '''
        reply, output_msgs = self._execute_code(code)

        # Even with rich output as false, we send plots
        data = output_msgs[0]['content']['data']
        self.assertEqual(len(data), 2, data.keys())
        self.assertEqual(data['text/plain'], 'plot without title')
        self.assertIn('image/png', data)

        # And reset
        code = 'options(jupyter.rich_display = TRUE)'
        reply, output_msgs = self._execute_code(code, tests=False)

    def test_irkernel_df_default_rich_output(self):
        """data.frame rich representation"""
        code = 'data.frame(x = 1:3)'
        reply, output_msgs = self._execute_code(code)
        
        # we currently send three formats: text/plain, html, and latex
        data = output_msgs[0]['content']['data']
        self.assertEqual(len(data), 4, data.keys())

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
    
    def test_warning_message(self):
        self.flush_channels()
        reply, output_msgs = self.execute_helper('warning(simpleWarning("wmsg"))')
        self.assertEqual(output_msgs[0]['msg_type'], 'stream')
        self.assertEqual(output_msgs[0]['content']['name'], 'stderr')
        self.assertEqual(output_msgs[0]['content']['text'], 'Warning message:\n“wmsg”')
        
        self.flush_channels()
        reply, output_msgs = self.execute_helper('f <- function() warning("wmsg"); f()')
        self.assertEqual(output_msgs[0]['msg_type'], 'stream')
        self.assertEqual(output_msgs[0]['content']['name'], 'stderr')
        self.assertEqual(output_msgs[0]['content']['text'], 'Warning message in f():\n“wmsg”')

    def test_should_increment_history(self):
        """properly increments execution history"""
        code = 'data.frame(x = 1:3)'
        reply, output_msgs = self._execute_code(code)
        reply2, output_msgs2 = self._execute_code(code)
        execution_count_1 = reply['content']['execution_count']
        execution_count_2 = reply2['content']['execution_count']
        self.assertEqual(execution_count_1 + 1, execution_count_2)

    def test_should_not_increment_history(self):
        """Does not increment history if silent is true or store_history is false"""
        code = 'data.frame(x = 1:3)'
        reply, output_msgs = self._execute_code(code, store_history=False)
        reply2, output_msgs2 = self._execute_code(code, store_history=False)
        reply3, output_msgs3 = self._execute_code(code, tests=False, silent=True)
        execution_count_1 = reply['content']['execution_count']
        execution_count_2 = reply2['content']['execution_count']
        execution_count_3 = reply3['content']['execution_count']
        self.assertEqual(execution_count_1, execution_count_2)
        self.assertEqual(execution_count_1, execution_count_3)

    def test_irkernel_inspects(self):
        """Test if object inspection works."""
        self.flush_channels()

        def test_token_is_ok(token, preprocess=None, postprocess=None):
            """Check if inspect_request for the `token` returns a reply.

            Run code in `preprocess` before requesting if it's given,
            and `proprocess` after requesting.

            Currently just test if the kernel replys without an error
            and not care about its content.
            Because the contents of inspections are still so arguable.
            When the requirements for the contents are decided,
            fix the tests beow and check the contents.
            """
            if preprocess:
                self._execute_code(preprocess, tests=False)

            msg_id = self.kc.inspect(token)
            reply = self.kc.get_shell_msg(timeout=TIMEOUT)
            validate_message(reply, 'inspect_reply', msg_id)

            self.assertEqual(reply['content']['status'], 'ok')
            self.assertTrue(reply['content']['found'])
            self.assertGreaterEqual(len(reply['content']['data']), 1)

            if postprocess:
                self._execute_code(postprocess, tests=False)

        # Numeric constant
        test_token_is_ok('1')
        # Reserved word
        test_token_is_ok('NULL')
        # Dataset with a help document
        test_token_is_ok('iris')
        # Function with a help document
        test_token_is_ok('c')
        # Function name with namespace
        test_token_is_ok('base::c')
        # Function not exported from namespace
        test_token_is_ok('tools:::.Rd2pdf')
        # User-defined variable
        test_token_is_ok(
            'x',
            preprocess='x <- 1',
            postprocess='rm("x")'
        )
        # User-defined function
        test_token_is_ok(
            'f',
            preprocess='f <- function (x) x + x',
            postprocess='rm("f")'
        )
        # Object which masks other object in workspace
        test_token_is_ok(
            'c',
            preprocess='c <- function (x) x + x',
            postprocess='rm("c")'
        )


if __name__ == '__main__':
    unittest.main(verbosity=2)
