const cmd_key = /Mac/.test(navigator.platform) ? 'Cmd' : 'Ctrl'

const edit_actions = [
	{
		name: 'R Assign',
		shortcut: 'Alt--',
		icon: 'fa-long-arrow-left',
		help: 'R: Inserts the left-assign operator (<-)',
		handler(cm) {
			cm.replaceSelection(' <- ')
		},
	},
	{
		name: 'R Pipe',
		shortcut: `Shift-${cmd_key}-M`,
		icon: 'fa-angle-right',
		help: 'R: Inserts the magrittr pipe operator (%>%)',
		handler(cm) {
			cm.replaceSelection(' %>% ')
		},
	},
	{
		name: 'R Help',
		shortcut: 'F1',
		icon: 'fa-book',
		help: 'R: Shows the manpage for the item under the cursor',
		handler(cm, cell) {
			const {anchor, head} = cm.findWordAt(cm.getCursor())
			const word = cm.getRange(anchor, head)
			
			const callbacks = cell.get_callbacks()
			const options = {silent: false, store_history: false, stop_on_error: true}
			cell.last_msg_id = cell.notebook.kernel.execute(`help(\`${word}\`)`, callbacks, options)
		},
	},
]

const prefix = 'irkernel'

function add_edit_shortcut(notebook, actions, keyboard_manager, edit_action) {
	const {name, shortcut, icon, help, handler} = edit_action
	
	const action = {
		icon, help,
		help_index : 'zz',
		handler: () => {
			const cell = notebook.get_selected_cell()
			handler(cell.code_mirror, cell)
		},
	}
	
	const full_name = actions.register(action, name, prefix)
	
	Jupyter.keyboard_manager.edit_shortcuts.add_shortcut(shortcut, full_name)
}

function render_math(pager, html) {
	if (!html) return
	const $container = pager.pager_element.find('#pager-container')
	$container.find('p[style="text-align: center;"]').map((i, e) =>
		e.outerHTML = `\\[${e.querySelector('i').innerHTML}\\]`)
	$container.find('i').map((i, e) =>
		e.outerHTML = `\\(${e.innerHTML}\\)`)
	MathJax.Hub.Queue(['Typeset', MathJax.Hub, $container[0]])
}

define(['base/js/namespace'], ({
	notebook,
	actions,
	keyboard_manager,
	pager,
}) => ({
	onload() {
		edit_actions.forEach(a => add_edit_shortcut(notebook, actions, keyboard_manager, a))
		
		pager.events.on('open_with_text.Pager', (event, {data: {'text/html': html}}) =>
			render_math(pager, html))
	},
}))
