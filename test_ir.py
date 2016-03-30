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

class DisplaySystem(jkt.KernelTests):
    kernel_name = 'ir'

    language_name = 'R'

    code_display_data = [
    {'code': 'options(jupyter.rich_display = FALSE);cat("a")', 'mime': {'text/plain':'a'}},
    {'code': '"a"', 'mime': {'text/plain':'"a"'}},
    {'code': '1:3', 'mime': {'text/plain':'[1] 1 2 3'}},
    ]

if __name__ == '__main__':
    unittest.main()
