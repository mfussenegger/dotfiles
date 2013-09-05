
\documentclass[a4paper, oneside, halfparskip, 11pt]{scrartcl}

\usepackage[ngerman]{babel}
\usepackage[utf8]{inputenc}

\usepackage{mathpazo}
\usepackage{textcomp}

\usepackage[left=2cm,right=2cm, top=2cm, bottom=2cm, includeheadfoot]{geometry}

\usepackage{graphicx}
\usepackage{amsmath}
\usepackage{listings}
\usepackage{paralist}

\usepackage{scrpage2}
\usepackage{array}
\usepackage{url}
\usepackage{lastpage}

\usepackage{setspace}

\ihead{IHEAD}
\chead{}
\ohead{AUTHOR}
\ifoot{}
\cfoot{\pagemark / \pageref{LastPage}}
\ofoot{}

\setheadsepline{1pt}
\setfootsepline{1pt}

\pagestyle{scrheadings}

\begin{document}

\begin{titlepage}
	\title{TITLE}
	\author{AUTHOR}

	\maketitle
\tableofcontents
\end{titlepage}

%\onehalfspacing

\label{LastPage}
\end{document}
