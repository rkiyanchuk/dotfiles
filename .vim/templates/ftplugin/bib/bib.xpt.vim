" AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
" Personal customizations to XPTemplates for LaTeX.

XPTemplate priority=personal+

let s:f = g:XPTfuncs()

XPTvar $SParg      ''
XPTvar $SPcmd      ''
XPTvar $SPop       ''

XPT article " Article
@ARTICLE{`key^,
    author = {{`author^}},
    title = {{`title^}},
    journal = {{`journal^}},
    year = {{`year^}},
    pages = {{`pages^}},
    volume = {{`volume^}},
    number = {{`^}},
    part = {{`^}},
    month = {{`^}},
    abstract = {{`^}},
    language = {`ukrainian^}
}
..XPT

XPT incollection " Incollection
@INCOLLECTION{`key^,
    author = {{`author^}},
    title = {{`title^}},
    booktitle = {{`booktitle^}},
    publisher = {{`publisher^}},
    year = {{`year^}},
    editor = {{`^}},
    pages = {{`^}},
    volume = {{`^}},
    number = {{`^}},
    series = {{`^}},
    chapter = {{`^}},
    address = {{`^}},
    edition = {{`^}},
    month = {{`^}},
    abstract = {{`^}},
    language = {`ukrainian^}
}

XPT patent " Patent
@PATENT{`key^,
    author = {{`^}},
    title = {{`^}},
    assignee = {{`^}},
    number = {{`^}},
    znumber = {{`^}},
    country = {{`^}},
    year = {{`^}},
    yearfiled = {{`^}},
    type = {{`^}},
    language = {`ukrainian^}
}
..XPT

XPT phdthesis " PhD Thesis
@PHDTHESIS{,
    author = {{`author^}},
    title = {{`title^}},
    school = {{`school^}},
    year = {{`year^}},
    type = {{`^}},
    address = {{`^}},
    language = {ukrainian}
}
..XPT

