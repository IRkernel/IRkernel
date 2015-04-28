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

if __name__ == '__main__':
    unittest.main()
