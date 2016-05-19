import unittest
import jupyter_kernel_test as jkt

class IRkernelTests(jkt.KernelTests):
    kernel_name = 'ir'

    language_name = 'R'

    code_hello_world = 'print("hello, world")'

    completion_samples = [
        {
            'text': 'zi',
            'matches': {'zip'},
        },
    ]

    complete_code_samples = ['1', 'print("hello, world")', 'f <- function(x) {\n  x*2\n}']
    incomplete_code_samples = ['print("hello', 'f <- function(x) {\n  x*2']

class InstallspecTests(jkt.KernelTests):
    # just a small test, it's the same kernel after all...
    kernel_name = 'testir'

    language_name = 'R'

    code_hello_world = 'print("hello, world")'

class DisplaySystemTests(jkt.KernelTests):
    kernel_name = 'ir'

    language_name = 'R'

    code_display_data = [
    {'code': 'options(jupyter.rich_display = FALSE);cat("a")', 'mime': {'text/plain':'a'}},
    {'code': '"a"', 'mime': {'text/plain':'"a"'}},
    {'code': '1:3', 'mime': {'text/plain':'[1] 1 2 3'}},
    ]

class AbstractIRKernel(jkt.KernelTests):
    kernel_name = 'ir'

    language_name = 'R'

    def _execute_code(self, code):
        self.flush_channels()

        reply, output_msgs = self.execute_helper(code)

        self.assertEqual(reply['content']['status'], 'ok')
        self.assertGreaterEqual(len(output_msgs), 1)
        # the irkernel only sends display_data, not execute_results
        self.assertEqual(output_msgs[0]['msg_type'], 'display_data')
        return reply, output_msgs

class PlotExistsTests(AbstractIRKernel):
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

class OnlyPNGPlotsTests(AbstractIRKernel):
        
    def test_irkernel_plots(self):
        code = """
options(jupyter.plot_mimetypes = 'image/png')
plot(1:3)
"""
        reply, output_msgs = self._execute_code(code)
        # Only png
        self.assertEqual(len(output_msgs[0]['content']['data']), 1)
        self.assertIn('image/png', output_msgs[0]['content']['data'])

class DefaultRichOutput(AbstractIRKernel):

    def test_irkernel_default_rich_output(self):
        code = """
data.frame(x = 1:3)
"""
        reply, output_msgs = self._execute_code(code)
        # we currently send three formats: text/plain, html, and latex
        self.assertEqual(len(output_msgs[0]['content']['data']), 3)

class NoRichOutput(AbstractIRKernel):

    def test_irkernel_no_rich_output(self):
        code = """
options(jupyter.rich_display = FALSE)
data.frame(x = 1:3)
"""
        reply, output_msgs = self._execute_code(code)
        # only text/plain
        self.assertEqual(len(output_msgs[0]['content']['data']), 1)

if __name__ == '__main__':
    unittest.main()
