import sys
import os
import re
import unittest
from warnings import simplefilter
from traceback import print_tb


class InlineTestResult(unittest.result.TestResult):
    def getDescription(self, test):
        name = re.match('.*[.]test_(\w+)', test.id()).group(1).replace('_', ' ')
        return test.shortDescription() or name

    def startTest(self, test):
        super().startTest(test)
        print(self.getDescription(test))

    def stopTest(self, test):
        super().stopTest(test)
        print(end='\0')
        sys.stdout.flush()

    def addSuccess(self, test):
        super().addSuccess(test)
        print('ok')

    def addError(self, test, err):
        super().addError(test, err)
        print('error')
        self.print_err(*err)

    def addFailure(self, test, err):
        super().addFailure(test, err)
        print('failure')
        self.print_err(*err)
    
    def print_err(self, cls, error, tb):
        print(error)
        print('Traceback:')
        print_tb(tb, file=sys.stdout)

    def addSkip(self, test, reason):
        super().addSkip(test, reason)
        print('skipped')

    def addExpectedFailure(self, test, err):
        super().addExpectedFailure(test, err)
        print('expected failure')

    def addUnexpectedSuccess(self, test):
        super().addUnexpectedSuccess(test)
        print('unexpected success')

    def printErrors(self):
        pass


if __name__ == '__main__':
    simplefilter(action='ignore', category=DeprecationWarning)
    unittest.main('test_ir', testRunner=unittest.TextTestRunner(resultclass=InlineTestResult, stream=open(os.devnull, 'w')))
