# Test manually via
# IR_KERNEL_NAME=ir python3 -m test_ir -k some_test

import os
import sys
from pathlib import Path

import unittest

from jupyter_client.manager import start_new_kernel
from jupyter_kernel_test import validate_message, KernelTests


without_rich_display = '''\
options(jupyter.rich_display = FALSE)
{}
options(jupyter.rich_display = TRUE)
'''
#this will not work!
#withr::with_options(list(jupyter.rich_display = FALSE), {})

TIMEOUT = 15


class IRkernelTests(KernelTests):
    kernel_name = os.environ.get('IR_KERNEL_NAME', 'testir')

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
        {'text': 'zi',                     'matches': {'zip'}},
        {'text': 'seq_len(',               'matches': {'length.out = '}},
        {'text': 'base::transform(',       'matches': {'`_data` = ', '...'}},
        {'text': 'foo(R_system',           'matches': {'R_system_version'}},
        {'text': 'version$plat',           'matches': {'version$platform'}},
        {'text': 'stats4::AIC@def',        'matches': {'stats4::AIC@default'}},
        {'text': 'stats4::AIC@default@ta', 'matches': {'stats4::AIC@default@target'}},
        {'text': 'grDevice',               'matches': {'grDevices::'}},
        {'text': 'base::abbrev',           'matches': {'base::abbreviate'}},
        {'text': 'base::.rowNamesD',       'matches': {'base::`.rowNamesDF<-`'}},
        {'text': 'repr:::repr_png.def',    'matches': {'repr:::repr_png.default'}},
        {'text': 'repr::format2repr$mark', 'matches': {'repr::format2repr$markdown'}},
        {'text': 'load("test_i',           'matches': {'test_ir.py'}},
        {'text': 'load("./test_',          'matches': {'./test_utils.r', './test_kernel.r', './test_ir.py'}},
        {'text': '.Last.v',                'matches': {'.Last.value'}},
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
        
        # we send image dimensions
        metadata = output_msgs[0]['content']['metadata']
        self.assertEqual(len(metadata), 1, metadata.keys())
        self.assertIn('image/png', metadata)
        self.assertEqual(metadata['image/png'], dict(width=420, height=420), metadata['image/png'])

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
        self.assertEqual(len(metadata), 1, metadata.keys())
        self.assertIn('image/png', metadata)
        self.assertEqual(metadata['image/png'], dict(width=420, height=420), metadata['image/png'])

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
        self.assertEqual(len(metadata['image/svg+xml']), 3)
        self.assertEqual(metadata['image/svg+xml'], dict(width=420, height=420, isolated=True), metadata['image/svg+xml'])

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
        reply, output_msgs = self.execute_helper('options(warn=1); warning(simpleWarning("wmsg"))')
        self.assertEqual(output_msgs[0]['msg_type'], 'stream')
        self.assertEqual(output_msgs[0]['content']['name'], 'stderr')
        self.assertEqual(output_msgs[0]['content']['text'].strip(), 'Warning message:\n“wmsg”')
        
        self.flush_channels()
        reply, output_msgs = self.execute_helper('options(warn=1); f <- function() warning("wmsg"); f()')
        self.assertEqual(output_msgs[0]['msg_type'], 'stream')
        self.assertEqual(output_msgs[0]['content']['name'], 'stderr')
        self.assertEqual(output_msgs[0]['content']['text'].strip(), 'Warning message in f():\n“wmsg”')

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

    irkernel_inspects = [
        dict(name='Numeric constant', token='1'),
        # dict(name='Reserved word', token='NULL'),  # null is not an object anymore?
        dict(name='Dataset with a help document', token='iris'),
        dict(name='Function with a help document', token='c'),
        dict(name='Function name with namespace', token='base::c'),
        dict(name='Function not exported from namespace', token='tools:::.Rd2pdf'),
        dict(name='User-defined variable', token='x', vars=dict(x='1')),
        dict(name='User-defined function', token='f', vars=dict(f='function (x) x + x')),
        dict(name='Object which masks other object in workspace', token='c', vars=dict(c='function (x) x + x')),
    ]

    def test_irkernel_inspects(self):
        """Test if object inspection works.

        Checks if inspect_request for rach `token` returns a reply.

        Currently just test if the kernel replys without an error and not care about its content.
        Because the contents of inspections are still so arguable.
        When the requirements for the contents are decided, fix the tests and check the contents.
        """
        self.flush_channels()

        for sample in self.irkernel_inspects:
            with self.subTest(text=sample["name"]):
                for var_name, var_code in sample.get('vars', {}).items():
                    self._execute_code(f'{var_name} <- {var_code}', tests=False)
    
                msg_id = self.kc.inspect(sample["token"])
                reply = self.kc.get_shell_msg(timeout=TIMEOUT)
                validate_message(reply, 'inspect_reply', msg_id)
    
                self.assertEqual(reply['content']['status'], 'ok')
                self.assertTrue(reply['content']['found'])
                self.assertGreaterEqual(len(reply['content']['data']), 1)
    
                for var_name in sample.get('vars', {}):
                    self._execute_code(f'rm("{var_name}")', tests=False)

    non_syntactic_completion_samples = [
        dict(text='xx$host[[1]]$h',
             matches=['xx$host[[1]]$h1', 'xx$host[[1]]$h2'],
             vars=dict(xx='list(host = list(list(h1 = 1, h2 = 2), list(h3 = 3, h4 = 4)))')),
        dict(text='odd_named_list$a',
             matches=['odd_named_list$a'],
             vars=dict(odd_named_list=r'list(a = 1, b = 2, `b c` = 3, `\`\\\`` = 4, 5)')),
        dict(text='odd_named_list$b',
             matches=['odd_named_list$b', 'odd_named_list$`b c`'],
             vars=dict(odd_named_list=r'list(a = 1, b = 2, `b c` = 3, `\`\\\`` = 4, 5)')),
        dict(text='odd_named_list$',
             matches=['odd_named_list$a', 'odd_named_list$b', 'odd_named_list$`b c`', r'odd_named_list$`\`\\\``', 'odd_named_list$'],
             vars=dict(odd_named_list=r'list(a = 1, b = 2, `b c` = 3, `\`\\\`` = 4, 5)')),
        dict(text='arith_named_env$a',
             matches=['arith_named_env$`abc+def`', 'arith_named_env$`abc-def`'],
             vars=dict(arith_named_env='list2env(list(`abc+def` = 1, `def-abc` = 2, `abc-def` = 3, defabc = 4))')),
        dict(text='arith_named_env$def',
             matches=['arith_named_env$`def-abc`', 'arith_named_env$defabc'],
             vars=dict(arith_named_env='list2env(list(`abc+def` = 1, `def-abc` = 2, `abc-def` = 3, defabc = 4))')),
        dict(text='arith_named_env$',
             matches=['arith_named_env$`abc+def`', 'arith_named_env$`def-abc`', 'arith_named_env$`abc-def`', 'arith_named_env$defabc'],
             vars=dict(arith_named_env='list2env(list(`abc+def` = 1, `def-abc` = 2, `abc-def` = 3, defabc = 4))')),
    ]

    def test_non_syntactic_completions(self):
        """Test tab-completion for non-syntactic names which require setup/teardown"""

        for sample in self.non_syntactic_completion_samples:
            with self.subTest(text=sample['text']):
                for var_name, var_value in sample['vars'].items():
                    self._execute_code(f'{var_name} <- {var_value}', tests=False)
                msg_id = self.kc.complete(sample['text'])
                reply = self.get_non_kernel_info_reply()
                validate_message(reply, 'complete_reply', msg_id)
                self.assertEqual(set(reply['content']['matches']), set(sample['matches']))

                for var_name in sample['vars']:
                    self._execute_code(f'rm({var_name})', tests=False)

if __name__ == '__main__':
    unittest.main(verbosity=2)
