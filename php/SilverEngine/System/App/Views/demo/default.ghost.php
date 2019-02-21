<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>SilverEngine Framework</title>

    <link rel="shortcut icon" type="image/png" href="{{ asset('images/icona.png') }}">
    <!-- spectre css -->
    <link rel="stylesheet" href="https://unpkg.com/spectre.css/dist/spectre.min.css">
    <link rel="stylesheet" href="https://unpkg.com/spectre.css/dist/spectre-exp.min.css">
    <link rel="stylesheet" href="https://unpkg.com/spectre.css/dist/spectre-icons.min.css">

    <style>
        html {
            position: relative;
            min-height: 100%;
        }

        body {
            background: -webkit-gradient(linear, left top, left bottom, from(#cbcdd2), to(#ffffff)) fixed;
            background-repeat: no-repeat;
            margin-bottom: 60px; /* Margin bottom by footer height */
        }

        a, a:hover, a:active, a:visited, a:focus {
            text-decoration: none;
            box-shadow: none;
            color: #45a3c4;

        }

        header {
            margin-top: 4rem;
        }

        header p {
            color: #7e7e7e;
        }

        footer {
            position: absolute;
            bottom: 0;
            left: 0;
            width: 100%;
        }

        footer p {
            margin: 1rem 0;
        }

        .nav-menu nav {
            margin-top: 2rem;
        }

        .nav-menu nav ul {
            list-style: none;
            margin: 0;
        }

        .nav-menu nav ul li {
            display: inline-block;
            height: 50px;
        }

        .nav-menu nav ul li a {
            display: block;
            padding: .5rem;
        }

        .nav-menu nav ul li a:hover {
            color: #05759c;
        }

        .nav-menu {
            background: radial-gradient(circle at center, #f2f2f2 0, transparent 90%);
        }

        .label {
            background: none;
            color: #47474b;
        }

        .logo {
            background: radial-gradient(circle at center, #f2f2f2 0, transparent 20%);
            padding: 20px 0px 1px 0px;
            margin-bottom: 50px;

        }
        header .logo span {
            font-size: 20px;
            letter-spacing: 0.7em;
            position: relative;
            left: 8px;
            top: -25px;
            color: #05759c;
            font-weight: 400;
        }
        .main{
            margin: 80px auto;
        }
    </style>
</head>
<body>

<div class="container grid-xl mt-2">

    <div class="label hide-sm mt-2 float-left">
            <b>Current branch: </b> <span> {{ $_branch_  ?: 'No git found!' }}</span>
    </div>

    <div class="label mt-2 float-right">
        <b>Version:</b> <span>1.0.5</span>
    </div>
    <div class="clearfix"></div>

    <div class="container grid-lg text-center">

        <header>
            <div class="columns">
                <div class="column col-12">
                    <div class="logo">
                        <img src="{{ asset('images/logo.png') }}" height="100">
                        <h1>SilverEngine</h1>
                        <span>Framework</span>
                    </div>
                    <p>Thanks for choosing SilverEngine !</p>
                </div>
            </div>
        </header>

        <main class="text-center nav-menu">
            <nav>
                <ul>
                    <li><a href="https://silverengine.net/">Changes</a></li>
                    <li><a href="https://silverengine.net/">News</a></li>
                    <li><a href="https://silverengine.net/docs">Documentation</a></li>
                    <li><a href="https://discord.gg/cwMygSP">Contribution</a></li>
                </ul>
            </nav>
        </main>

        <div class="main">
            <a href="https://discord.gg/cwMygSP" target="_blank">
                <img src="https://steemitimages.com/DQmbidwaM258YWuVUR6qvWdqKCnXw8pUsV6wxbUmgD38BBq/0_lq-C3-gIuW9L9xxn.png" width="200">
            </a>
            <p>Join us on Community HUB</p>
        </div>

        <footer class="text-center">
            <div class="col-12">
                <p>SilverEngine Team &copy;2017-2019</p>
            </div>
        </footer>


    </div>


</div>


</body>
</html>
