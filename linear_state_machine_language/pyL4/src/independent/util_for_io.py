def writeFile(path:str, contents:str) -> None:
    f = open(path, 'w', encoding='utf8')
    f.write(contents)
    f.close()


def writeReadOnlyFile(path:str, contents:str) -> None:
    from os import system
    system(f'chmod u+w {path}')
    writeFile(path, contents)
    system(f'chmod a-w {path}')