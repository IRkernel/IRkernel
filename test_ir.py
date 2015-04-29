import unittest
import kerneltest

class IRkernelTests(kerneltest.KernelTests):
    kernel_name = "ir"

    language_name = "R"

    code_hello_world = "print('hello, world')"

    completion_samples = [
        {
            'text': 'zi',
            'matches': {'zip'},
        },
    ]

    complete_code_samples = ['1', "print('hello, world')", "f <- function(x) {\n  x*2\n}"]
    incomplete_code_samples = ["print('hello", "f <- function(x) {\n  x*2"]

if __name__ == '__main__':
    unittest.main()
