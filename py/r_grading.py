from IPython.core.display import HTML

def viewQuestion(notebook, num) :
    """ viewQuestion 'num' in R 'notebook' (html string) """ 
    num = str(num)
    portions = notebook.split('<pre class="r">')
    code = [p for p in portions if '### Question ' + num in p]
    return HTML('<pre class="r">' + code[0])

def grade(s, num, earned, possible, comment = '') :
    """ grades notebook 's' for question 'num' """

    q = '### Question ' + str(num) 

    earned = str(earned)
    possible = str(possible)

    emoji = '&#9989'
    if earned != possible :
        emoji = '&#10071'

    repl = q + '\n' 
    repl += '<p style = "color:red; background-color:white; padding:5px; border: solid 1px;">' + 'Question ' + str(num)
    repl += ' --  [' + earned + ' / ' + possible + ' points] ' + emoji 

    if comment != '' :
        repl += '\n' + comment.strip() 

    repl += '</p>'
    s = s.replace(q, repl)
    return(s)


def inputMultiLine(prompt, end = 'qqq') :
    print(prompt + '(' + end + ' to end)', end = ' ')
    x = input()
    if (x.strip() == '' or x.strip() == 'qqq') :
        return ''

    xx = []
    while True :
        xx.append(x)
        x = input()
        if x.strip() == 'qqq' :
            return '\n'.join(xx)
