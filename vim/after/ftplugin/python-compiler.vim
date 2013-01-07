<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'>
<head>
<title>vim-plugins - my vim plugins</title>
<meta name='generator' content='cgit v0.8.2.1-16-g5c967'/>
<meta name='robots' content='index, nofollow'/>
<link rel='stylesheet' type='text/css' href='/cgit.css'/>
<link rel='shortcut icon' href='/phrak.png'/>
<link rel='alternate' title='Atom feed' href='http://code.phraktured.net/cgit.cgi/vim-plugins/atom/python-compiler.vim?h=master' type='application/atom+xml'/></head>
<body>
<table id='header'>
<tr>
<td class='logo' rowspan='2'><a href='/cgit.cgi/'><img src='/phrak.png' alt='cgit logo'/></a></td>
<td class='main'><a href='/cgit.cgi/'>index</a> : <a title='vim-plugins' href='/cgit.cgi/vim-plugins/'>vim-plugins</a></td><td class='form'><form method='get' action=''>
<select name='h' onchange='this.form.submit();'>
<option value='master' selected='selected'>master</option>
</select> <input type='submit' name='' value='switch'/></form></td></tr>
<tr><td class='sub'>my vim plugins</td><td class='sub right'></td></tr></table>
<table class='tabs'><tr><td>
<a href='/cgit.cgi/vim-plugins/'>summary</a><a href='/cgit.cgi/vim-plugins/refs/'>refs</a><a href='/cgit.cgi/vim-plugins/log/'>log</a><a class='active' href='/cgit.cgi/vim-plugins/tree/'>tree</a><a href='/cgit.cgi/vim-plugins/commit/'>commit</a><a href='/cgit.cgi/vim-plugins/diff/'>diff</a><a href='/cgit.cgi/vim-plugins/stats/'>stats</a></td><td class='form'><form class='right' method='get' action='/cgit.cgi/vim-plugins/log/python-compiler.vim'>
<select name='qt'>
<option value='grep'>log msg</option>
<option value='author'>author</option>
<option value='committer'>committer</option>
</select>
<input class='txt' type='text' size='10' name='q' value=''/>
<input type='submit' value='search'/>
</form>
</td></tr></table>
<div class='content'>path: <a href='/cgit.cgi/vim-plugins/tree/?h=master'>root</a>/<a href='/cgit.cgi/vim-plugins/tree/python-compiler.vim'>python-compiler.vim</a> (<a href='/cgit.cgi/vim-plugins/plain/python-compiler.vim'>plain</a>)<br/>blob: 19cd2ce1fa8ab5cd3baeb7ff4ab658e4ab43288d
<table summary='blob content' class='blob'>
<tr><td class='linenumbers'><pre><a class='no' id='n1' name='n1' href='#n1'>1</a>
<a class='no' id='n2' name='n2' href='#n2'>2</a>
<a class='no' id='n3' name='n3' href='#n3'>3</a>
<a class='no' id='n4' name='n4' href='#n4'>4</a>
<a class='no' id='n5' name='n5' href='#n5'>5</a>
<a class='no' id='n6' name='n6' href='#n6'>6</a>
<a class='no' id='n7' name='n7' href='#n7'>7</a>
<a class='no' id='n8' name='n8' href='#n8'>8</a>
<a class='no' id='n9' name='n9' href='#n9'>9</a>
<a class='no' id='n10' name='n10' href='#n10'>10</a>
<a class='no' id='n11' name='n11' href='#n11'>11</a>
<a class='no' id='n12' name='n12' href='#n12'>12</a>
<a class='no' id='n13' name='n13' href='#n13'>13</a>
<a class='no' id='n14' name='n14' href='#n14'>14</a>
<a class='no' id='n15' name='n15' href='#n15'>15</a>
<a class='no' id='n16' name='n16' href='#n16'>16</a>
<a class='no' id='n17' name='n17' href='#n17'>17</a>
<a class='no' id='n18' name='n18' href='#n18'>18</a>
<a class='no' id='n19' name='n19' href='#n19'>19</a>
<a class='no' id='n20' name='n20' href='#n20'>20</a>
<a class='no' id='n21' name='n21' href='#n21'>21</a>
<a class='no' id='n22' name='n22' href='#n22'>22</a>
<a class='no' id='n23' name='n23' href='#n23'>23</a>
<a class='no' id='n24' name='n24' href='#n24'>24</a>
<a class='no' id='n25' name='n25' href='#n25'>25</a>
<a class='no' id='n26' name='n26' href='#n26'>26</a>
<a class='no' id='n27' name='n27' href='#n27'>27</a>
<a class='no' id='n28' name='n28' href='#n28'>28</a>
<a class='no' id='n29' name='n29' href='#n29'>29</a>
</pre></td>
<td class='lines'><pre><code>" vim compiler file
" Compiler:		Python     
" Maintainer:   Aaron Griffin &lt;aaronmgriffin-at-gmail-com&gt;
" Last Change:  04 Jan 2006

if exists("current_compiler")
  finish
endif
let current_compiler = "python"

let s:cpo_save = &amp;cpo
set cpo-=C

setlocal makeprg=python\ -c\ \"import\ py_compile;\ py_compile.compile(r'%')\"

setlocal errorformat=
	\%A\ \ File\ \"%f\"\\\,\ line\ %l\\\,%m,
	\%C\ \ \ \ %.%#,
	\%+Z%.%#Error\:\ %.%#,
	\%A\ \ File\ \"%f\"\\\,\ line\ %l,
	\%+C\ \ %.%#,
	\%-C%p^,
	\%Z%m,
	\%-G%.%#

let &amp;cpo = s:cpo_save
unlet s:cpo_save

"vim: ft=vim</code></pre></td></tr></table>
</div><div class='footer'>generated  by cgit v0.8.2.1-16-g5c967 at 2009-08-17 21:19:25 (GMT)</div>
</body>
</html>
