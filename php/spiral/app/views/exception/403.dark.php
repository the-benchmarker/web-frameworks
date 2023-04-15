<extends:layout.base title="[[Access Forbidden]]"/>
<use:element path="embed/links" as="homepage:links"/>

<stack:push name="styles">
    <link rel="stylesheet" href="/styles/welcome.css"/>
</stack:push>

<define:body>
    <div class="wrapper">
        <div class="placeholder">
            <img src="/images/403.svg" alt="Error 403" width="300px"/>
            <h2>[[Access Forbidden]]</h2>

            <homepage:links git="https://github.com/spiral/app" style="font-weight: bold;"/>

            <div style="font-size: 12px; margin-top: 10px;">
                [[This view file is located in]] <b>app/views/exception/403.dark.php</b>.
            </div>
        </div>
    </div>
</define:body>
