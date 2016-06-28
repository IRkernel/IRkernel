import unittest
import jupyter_kernel_test as jkt

from jupyter_client.manager import start_new_kernel


reset_rich_display = "options(jupyter.rich_display = FALSE);%s;options(jupyter.rich_display = TRUE);"



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

        self.assertEqual(reply['content']['status'], 'ok')
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
    {'code': (reset_rich_display%'"a"'), 'mime': 'text/plain'},
    {'code': (reset_rich_display%'1:3'), 'mime': 'text/plain'},
    ]

    def test_display_vector(self):
        code = "1:3"
        reply, output_msgs = self._execute_code(code)
        # we currently send three formats: text/plain, text/html, text/latex, and text/markdown
        self.assertEqual(len(output_msgs[0]['content']['data']), 4)
        self.assertEqual(output_msgs[0]['content']['data']['text/plain'], "[1] 1 2 3")
        self.assertIn('text/html', output_msgs[0]['content']['data'])
        # this should not be a test of the repr functionality...
        self.assertIn("</li>", output_msgs[0]['content']['data']['text/html'])
        self.assertIn('text/latex', output_msgs[0]['content']['data'])
        self.assertIn('text/markdown', output_msgs[0]['content']['data'])

    def test_display_vector_only_plaintext(self):
        code = reset_rich_display%"1:3"
        reply, output_msgs = self._execute_code(code)
        self.assertEqual(len(output_msgs[0]['content']['data']), 1)
        self.assertEqual(output_msgs[0]['content']['data']['text/plain'], "[1] 1 2 3")

    def test_irkernel_plots(self):
        code = "plot(1:3)"
        reply, output_msgs = self._execute_code(code)
        # we currently send three formats: png, svg and text/plain
        self.assertEqual(len(output_msgs[0]['content']['data']), 3)
        self.assertEqual(output_msgs[0]['content']['data']['text/plain'], "plot without title")
        self.assertIn('image/svg+xml', output_msgs[0]['content']['data'])
        self.assertIn('image/png', output_msgs[0]['content']['data'])
        # we isolate only svg plots
        self.assertEqual(len(output_msgs[0]['content']['metadata']), 1)
        self.assertEqual(len(output_msgs[0]['content']['metadata']['image/svg+xml']), 1)
        self.assertEqual(output_msgs[0]['content']['metadata']['image/svg+xml']['isolated'], True)

    def test_irkernel_plots_only_PNG(self):
        # the reset needs to happen in another execute because plots are sent after either
        # the next plot is opened or everything is executed, not at the time when plot
        # command is actually happening.
        # (we have a similar problem with width/... but we worked around it by setting the
        # appropriate options to the recorderdplot object)
        code = """
old_options <- options(jupyter.plot_mimetypes = c('image/png'))
plot(1:3)
"""
        reply, output_msgs = self._execute_code(code)
        # Only png, no svg or plain/text
        self.assertEqual(len(output_msgs[0]['content']['data']), 1)
        self.assertIn('image/png', output_msgs[0]['content']['data'])
        # And reset
        code = "options(old_options)"
        reply, output_msgs = self._execute_code(code, tests=False)

    def test_irkernel_df_default_rich_output(self):
        code = "data.frame(x = 1:3)"
        reply, output_msgs = self._execute_code(code)
        # we currently send three formats: text/plain, html, and latex
        self.assertEqual(len(output_msgs[0]['content']['data']), 3)

    def test_irkernel_df_no_rich_output(self):
        code = """
options(jupyter.rich_display = FALSE)
data.frame(x = 1:3)
options(jupyter.rich_display = TRUE)
"""
        reply, output_msgs = self._execute_code(code)
        # only text/plain
        self.assertEqual(len(output_msgs[0]['content']['data']), 1)

    def test_in_kernel_set(self):
        reply, output_msgs = self._execute_code("getOption('jupyter.in_kernel')")
        self.assertEqual(output_msgs[0]['content']['data']['text/plain'], "[1] TRUE")

if __name__ == '__main__':
    unittest.main()
