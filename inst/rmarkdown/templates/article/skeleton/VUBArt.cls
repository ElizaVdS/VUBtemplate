%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VUB Article
% Class File
% Version 1.0 (2021-02-01)
%
% Modified from:
% Stylish Article
% Class File
% Version 2.2 (2020-10-22)
%
% This template has been downloaded from:
% http://www.LaTeXTemplates.com
%
% Original author:
% Mathias Legrand (legrand.mathias@gmail.com)
% With extensive modifications by:
% Vel (vel@latextemplates.com)
%
% License:
% CC BY-NC-SA 3.0 (http://creativecommons.org/licenses/by-nc-sa/3.0/)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------------------------
%	CLASS CONFIGURATION
%----------------------------------------------------------------------------------------

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{VUBArt}[01/02/2021, v1.0]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}} % Pass through any options to the base class
\ProcessOptions* % Process given options

\LoadClass{article} % Load the base class

%----------------------------------------------------------------------------------------
%	REQUIRED PACKAGES AND DOCUMENT CONFIGURATIONS
%----------------------------------------------------------------------------------------

\RequirePackage{ifthen}
\RequirePackage{calc}
\RequirePackage{amsmath, amsfonts, amssymb}
\RequirePackage{graphicx}
\graphicspath{{Figures/}{./}} % Specifies where to look for included images (trailing slash required)
\RequirePackage{natbib}  % Advanced bibliography
\RequirePackage{xcolor}
\RequirePackage{booktabs}
\RequirePackage{lastpage}
\AtEndOfClass{\RequirePackage{microtype}}

\RequirePackage{ifpdf}      % Needed to pick between latex and pdflatex

\flushbottom % Makes all text pages the same height

\RequirePackage[dutch, english, french]{babel}
\frenchbsetup{StandardLists=true}

\RequirePackage{url}

\RequirePackage{epstopdf}

\newcommand{\code}[1]{\begingroup \ttfamily #1\endgroup}% code command
\def\Var{\mathop{\mathrm{Var}}} % Variance

%----------------------------------------------------------------------------------------
%	FONTS
%----------------------------------------------------------------------------------------

\RequirePackage[utf8]{inputenc}  % For the accents
\RequirePackage[T1]{fontenc}   % For the accents
\RequirePackage{AlegreyaSans}  % Loads the Times Roman Fonts
\RequirePackage{mathptmx} % Loads the Times Roman Math Fonts

%----------------------------------------------------------------------------------------
%	MARGINS
%----------------------------------------------------------------------------------------

\RequirePackage[
	left=2cm,
	right=2cm,
	top=2.25cm,
	bottom=2.25cm,
	headheight=12pt,
	 a4paper,
]{geometry}

%----------------------------------------------------------------------------------------
%	FIGURE AND TABLE CAPTIONS
%----------------------------------------------------------------------------------------

\RequirePackage[
	labelfont={bf,sf,small},
	labelsep=period,
	justification=raggedright,
]{caption}

\setlength{\abovecaptionskip}{3pt}
\setlength{\belowcaptionskip}{0pt}

%----------------------------------------------------------------------------------------
%	PAGE HEADER
%----------------------------------------------------------------------------------------

\RequirePackage{fancyhdr} % Needed to define custom headers/footers
\pagestyle{fancy} % Enables the custom headers/footers

\renewcommand{\headrulewidth}{0pt} % No header rule
\renewcommand{\footrulewidth}{0pt} % No footer rule

% Headers
\lhead{}
\chead{}
\rhead{\small\sffamily\bfseries\@PaperTitle\ --- \thepage/\pageref{LastPage}}

% Footers
\lfoot{}
\cfoot{}
\rfoot{}

%----------------------------------------------------------------------------------------
%	SECTION SETUP
%----------------------------------------------------------------------------------------

\RequirePackage[explicit]{titlesec}

\titleformat{\section}
	{\color{color1}\large\sffamily\bfseries}
	{}
	{0em}
	{\colorbox{color1!10}{\parbox{\dimexpr\linewidth-2\fboxsep\relax}{\centering\thesection.\space#1}}}
	[]

\titleformat{name=\section,numberless}
	{\color{color1}\large\sffamily\bfseries}
	{}
	{0em}
	{\colorbox{color1!10}{\parbox{\dimexpr\linewidth-2\fboxsep\relax}{\centering#1}}}
	[]

\titleformat{\subsection}
	{\color{color2}\sffamily\bfseries}
	{\thesubsection}
	{0.5em}
	{#1}
	[]

\titleformat{\subsubsection}
	{\color{color1}\sffamily\small\bfseries}
	{\thesubsubsection}
	{0.5em}
	{#1}
	[]

\titleformat{\paragraph}[runin]
	{\sffamily\small\bfseries}
	{}
	{0em}
	{#1}

\titlespacing*{\section}{0pc}{3ex \@plus4pt \@minus3pt}{5pt}
\titlespacing*{\subsection}{0pc}{2.5ex \@plus3pt \@minus2pt}{0pt}
\titlespacing*{\subsubsection}{0pc}{2ex \@plus2.5pt \@minus1.5pt}{0pt}
\titlespacing*{\paragraph}{0pc}{1.5ex \@plus2pt \@minus1pt}{10pt}

%----------------------------------------------------------------------------------------
%	TABLE OF CONTENTS SETUP
%----------------------------------------------------------------------------------------

\newlength{\tocsep}
\setlength\tocsep{1.5pc} % Sets the indentation of the sections in the table of contents
\setcounter{tocdepth}{3} % Three levels in the table of contents section: sections, subsections and subsubsections

\usepackage{titletoc}
\contentsmargin{0cm}

\titlecontents{section}[\tocsep]
	{\addvspace{4pt}\small\bfseries\sffamily}
	{\contentslabel[\thecontentslabel]{\tocsep}}
	{}
	{\hfill\thecontentspage}
	[]

\titlecontents{subsection}[\tocsep]
	{\addvspace{2pt}\sffamily}
	{\contentslabel[\thecontentslabel]{\tocsep}}
	{}
	{\ \titlerule*[.5pc]{.}\ \thecontentspage}
	[]

\titlecontents*{subsubsection}[\tocsep]
	{\footnotesize\sffamily}
	{}
	{}
	{}
	[\ \textbullet\ ]

%----------------------------------------------------------------------------------------
%	MULTIPLE AUTHOR SET
%----------------------------------------------------------------------------------------

\newcount\@authcnt
\newcount\@tmpcnt\@tmpcnt\z@

\def\@affiliation{%
	\ifnum\@tmpcnt<\@authcnt
		 \global\advance\@tmpcnt1
		\raggedright \csname @auth\romannumeral\the\@tmpcnt\endcsname\hfill\\%
		 \let\next\@affiliation \vskip1pt
	\else
		\let\next\relax
	\fi
	\next%
}

\newcommand{\affiliation}[1]{%
	\global\advance\@authcnt1
	\expandafter\gdef\csname @auth\romannumeral\the\@authcnt\endcsname
	{#1}
}

%----------------------------------------------------------------------------------------
%	LISTS
%----------------------------------------------------------------------------------------

\RequirePackage{enumitem}
 \setlist[itemize]{noitemsep,nolistsep}
  \setlist[enumerate]{noitemsep,nolistsep}

%----------------------------------------------------------------------------------------
%	ABSTRACT AND AUTHOR FRAME
%----------------------------------------------------------------------------------------

\newcommand{\PaperTitle}[1]{\def\@PaperTitle{#1}}
\newcommand{\Archive}[1]{\def\@Archive{#1}}
\newcommand{\Authors}[1]{\def\@Authors{#1}}
\newcommand{\JournalInfo}[1]{\def\@JournalInfo{#1}}
\newcommand{\Abstract}[1]{\def\@Abstract{#1}}
\newcommand{\Keywords}[1]{\def\@Keywords{#1}}

% ---------------------------------------------------------------------

\renewcommand{\@maketitle}{%
	\twocolumn[{%
		\thispagestyle{empty}%
		\vskip-36pt%
		{\raggedleft\small\sffamily\bfseries\@JournalInfo\\\@Archive\par}%
		\vskip20pt%
		{\raggedleft\color{color1}\sffamily\bfseries\fontsize{20}{25}\selectfont \@PaperTitle\par}%
		\vskip10pt%
		{\raggedleft\color{color2}\sffamily\bfseries\fontsize{12}{16}\selectfont \@Authors\par}%
		\vskip18pt%
		\fcolorbox{color2}{white}{%
			\parbox{\textwidth-2\fboxsep-2\fboxrule}{\centering%
				\colorbox{color1!10}{%
					\parbox{\textwidth-4\fboxsep-2\fboxrule}{%
						\ifx\@Keywords\@empty%
							\sffamily\textbf{\abstractname}\\\@Abstract%
						\else%
							\sffamily\textbf{\abstractname}\\\@Abstract\\[4pt]%
							\textbf{\keywordname}\\\@Keywords%
						\fi%
					}%
				}%
				\vskip4pt%
				\begingroup%
					\raggedright\sffamily\small%
					\footnotesize\@affiliation\par%
				\endgroup%%
			}%
		}%
		\vskip25pt%
	}]%
}

%----------------------------------------------------------------------------------------
%	REFERENCES
%----------------------------------------------------------------------------------------

\renewcommand{\@biblabel}[1]{\bfseries\color{color1}\textsuperscript{[#1]}} % Styling of the reference numbers in the bibliography
%\setlength{\bibitemsep}{0cm}

\let\oldbibliography\thebibliography

\renewcommand{\thebibliography}[1]{%
	\addcontentsline{toc}{section}{\refname}%
	\oldbibliography{#1}%
	\setlength\itemsep{0pt}%
}
