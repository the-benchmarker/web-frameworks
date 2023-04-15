<!DOCTYPE html>
<html lang="@{locale}">
<head>
    <title>${title}</title>
    <block:head>
        <stack:collect name="styles" level="2"/>
    </block:head>
</head>
<body>
<block:body/>
<stack:collect name="scripts" level="1"/>
</body>
<hidden>${context}</hidden>
</html>