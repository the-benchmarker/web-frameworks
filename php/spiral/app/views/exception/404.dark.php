<extends:layout.base title="[[Page not found]]"/>
<use:element path="embed/links" as="homepage:links"/>

<stack:push name="styles">
    <link rel="stylesheet" href="/styles/welcome.css"/>
</stack:push>

<define:body>
    <div class="wrapper">
        <div class="placeholder">
            <img src="/images/404.svg" alt="Error 404" width="300px"/>
            <h2>[[Page not found]]</h2>

            <homepage:links git="https://github.com/spiral/app" style="font-weight: bold;"/>

            <div style="font-size: 12px; margin-top: 10px;">
                [[This view file is located in]] <b>app/views/exception/404.dark.php</b>.
            </div>
        </div>
    </div>
</define:body>
