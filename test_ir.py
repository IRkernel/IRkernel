import unittest
import jupyter_kernel_test as jkt

from jupyter_client.manager import start_new_kernel

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


class IndependendTests(AbstractIRKernel):

    """This contains tests cases which do not alter the kernel environment.

    They are all in one class so that unittest test case discovery does not
    discover and skip over the default cases all the time...
    """

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


    def test_irkernel_default_rich_output(self):
        code = """
data.frame(x = 1:3)
"""
        reply, output_msgs = self._execute_code(code)
        # we currently send three formats: text/plain, html, and latex
        self.assertEqual(len(output_msgs[0]['content']['data']), 3)

    def test_in_kernel_set(self):
        reply, output_msgs = self._execute_code("getOption('jupyter.in_kernel')")
        self.assertEqual(output_msgs[0]['content']['data']['text/plain'], "[1] TRUE")


class OptionsDependendTests(AbstractIRKernel):
    """Test cases which need to get a new kernel because the options are changed"""

    def setUp(self):
        self.km, self.kc = start_new_kernel(kernel_name=self.kernel_name)

    def tearDown(self):
        self.kc.stop_channels()
        self.km.shutdown_kernel()

    # overwrite defaults to prevent errors on already shutdown kernels
    @classmethod
    def setUpClass(cls):
        pass

    @classmethod
    def tearDownClass(cls):
        pass


    code_display_data = [
    {'code': 'options(jupyter.rich_display = FALSE);cat("a")', 'mime': {'text/plain':'a'}},
    {'code': '"a"', 'mime': {'text/plain':'"a"'}},
    {'code': '1:3', 'mime': {'text/plain':'[1] 1 2 3'}},
    ]

    def test_irkernel_only_PNG_plots(self):
        code = """
options(jupyter.plot_mimetypes = 'image/png')
plot(1:3)
"""
        reply, output_msgs = self._execute_code(code)
        # Only png
        self.assertEqual(len(output_msgs[0]['content']['data']), 1)
        self.assertIn('image/png', output_msgs[0]['content']['data'])


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
