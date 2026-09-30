switch("path", "$projectDir/../src")
from os import parentDir, `/`

let basepath = projectDir().parentDir()
switch "define", "supranimBasePath:" & basepath
switch "define", "ssl"