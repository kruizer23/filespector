---
title: FILESPECTOR
section: 1
header: User Manual
footer: filespector 1.0.0
date: May 7, 2022
---
# NAME
filespector - Find text occurrences in multiple files

# SYNOPSIS
**filespector** [*OPTIONS*]

# DESCRIPTION
**filespector** GUI tool for quickly finding all text occurrences in
multiple files in a directory, matching a specific file extension pattern.

# OPTIONS
**-?**, **\-\-help**
: Show help options.

**-a**, **\-\-autostart**
: Automatically start searching after opening the application.

**-d** [*dir*], **\-\-directory**[=*dir*]
: Set the base directory to search from. Surround with double-quotes, if it contains spaces.

**-i** [*off*|*on*], **\-\-ignore-case**[=*off*|*on*]
: Ignore case sensitivity of the text to search for.

**-l** [*code*], **\-\-lang**[=*code*]
: Set the desired user interface language via the language code (i.e. \'en\', \'de\').

**-p** [*pattern*], **\-\-pattern**[=*pattern*]
: List of file extension patterns to match. Should be surrounded by  double-quotes. Seperate multiple entries with a \'|\' (e.g. \"\*.txt|\*.log\").

**-r** [*off*|*on*], **\-\-recursive**[=*off*|*on*]
: Recurse into subdirectories of the selected search directory.

**-s** [*text*], **\-\-searchterm**[=*text*]
: The text to search for. Surround by double-quotes, if it contains spaces or other special characters.

# EXAMPLES
**filespector -i on -r off -d \/home\/user -p \"\*.txt\|\*.log\" -s \"text to find\"**
: Case insensitive recursive search in directory \"\/home\/user\" for the term \"text to find\" in files with a \*.txt or \*.log extension.

**filespector \-\-lang de \-\-directory=\"\/home\/user\/my files\" \-\-searchterm=findme**
: Use the german language on the user interface and search for the term \"findme\" in the files in directory \"\/home\/user\/my files\".

# AUTHORS
Written by Frank Voorburg.

# BUGS
Submit bug reports online at: <https://github.com/kruizer23/filespector/issues>

# SEE ALSO
Full documentation and sources at: <https://github.com/kruizer23/filespector>

