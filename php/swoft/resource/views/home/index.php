<!doctype html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport"
        content="width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0">
  <meta http-equiv="X-UA-Compatible" content="ie=edge">
  <title>Swoft Framework 2.0 - PHP microservices coroutine framework</title>
  <style type="text/css">
    *,
    *::before,
    *::after {
      box-sizing: border-box;
    }

    html {
      font-family: sans-serif;
      line-height: 1.15;
      -webkit-text-size-adjust: 100%;
      -ms-text-size-adjust: 100%;
      -ms-overflow-style: scrollbar;
      -webkit-tap-highlight-color: transparent;
    }

    @-ms-viewport {
      width: device-width;
    }

    article, aside, dialog, figcaption, figure, footer, header, hgroup, main, nav, section {
      display: block;
    }

    body {
      margin: 0;
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
      font-size: 1rem;
      font-weight: 400;
      line-height: 1.5;
      color: #212529;
      text-align: left;
      background-color: #fefefe;
    }

    [tabindex="-1"]:focus {
      outline: 0 !important;
    }

    .container {
      width: 100%;
      padding-right: 15px;
      padding-left: 15px;
      margin-right: auto;
      margin-left: auto;
    }

    @media (min-width: 576px) {
      .container {
        max-width: 540px;
      }
    }

    @media (min-width: 768px) {
      .container {
        max-width: 720px;
      }
    }

    @media (min-width: 992px) {
      .container {
        max-width: 960px;
      }
    }

    @media (min-width: 1200px) {
      .container {
        max-width: 1140px;
      }
    }

    [hidden] {
      display: none !important;
    }

    h1, h2, h3, h4, h5, h6,
    .h1, .h2, .h3, .h4, .h5, .h6 {
      margin-bottom: 0.5rem;
      font-family: inherit;
      font-weight: 500;
      line-height: 1.2;
      color: inherit;
    }

    h1, .h1 {
      font-size: 2.5rem;
    }

    h2, .h2 {
      font-size: 2rem;
    }

    h3, .h3 {
      font-size: 1.75rem;
    }

    h4, .h4 {
      font-size: 1.5rem;
    }

    h5, .h5 {
      font-size: 1.25rem;
    }

    h6, .h6 {
      font-size: 1rem;
    }

    hr {
      margin-top: 1rem;
      margin-bottom: 1rem;
      border: 0;
      border-top: 1px solid rgba(0, 0, 0, 0.1);
    }

    .clearfix:after, .clearfix:before {
      content: " ";
      display: table
    }

    .clearfix:after {
      clear: both
    }

    code,
    kbd,
    pre,
    samp {
      font-family: SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace;
    }

    pre {
      display: block;
      font-size: 87.5%;
      color: #212529;
    }

    pre code {
      font-size: inherit;
      color: inherit;
      word-break: normal;
    }

    .pre-scrollable {
      max-height: 340px;
      overflow-y: scroll;
    }

    code {
      font-size: 87.5%;
      color: #e83e8c;
      word-break: break-word;
    }

    a {
      color: #777;
    }

    a:hover {
      color: #18bc9c;
    }

    a > code {
      color: inherit;
    }

    .swoft-title {
      margin-top: 28%;
      text-align: center;
      color: #18bc9c;
      opacity: 0.7;
      font: 8em 'Helvetica Neue';
      font-weight: 200;
      -webkit-box-reflect: below 3px linear-gradient(transparent, transparent 30%, rgba(0, 0, 0, .4));
    }

    ul, li {
      list-style: none;
      padding: 0;
      margin: 0;
    }

    .top-nav {
      margin-top: 5em;
    }

    .top-nav li, .foot-nav li {
      width: 25%;
      float: left;
    }

    .top-nav a, .foot-nav a {
      text-align: center;
      display: block;
      font-size: 2em;
      font-weight: 200;
      text-decoration: none;
    }

    .foot-nav {
      position: absolute;
      bottom: 5em;
      width: 100%;
    }

    .foot-nav a {
      color: #ccc;
    }
  </style>
</head>
<body>
<div class="container">
  <nav class="top-nav ">
    <ul class="clearfix">
      <li><a target="_blank"  href="https://github.com/swoft-cloud/swoft">Github</a></li>
      <li><a target="_blank"  href="https://swoft.org/docs">Document</a></li>
      <li><a target="_blank"  href="https://swoft.org/docs">Swoft.org</a></li>
      <li><a target="_blank"  href="https://gitter.im/swoft-cloud/community">Gitter.im</a></li>
    </ul>
  </nav>
  <div class="main">
    <h1 class="swoft-title">Swoft Framework
      <small>2.x</small>
    </h1>
  </div>
</div>
<nav class="foot-nav">
  <ul class="clearfix">
    <li><a target="_blank"  href="https://github.com/swoft-cloud/swoft">Github</a></li>
    <li><a target="_blank"  href="https://swoft.org/docs">Document</a></li>
    <li><a target="_blank"  href="https://swoft.org/docs">Swoft.org</a></li>
    <li><a target="_blank"  href="https://gitter.im/swoft-cloud/community">Gitter.im</a></li>
  </ul>
</nav>
</body>
</html>
