<?php
/**
 * @var \Swoft\View\Renderer $this
 */
?>
<!doctype html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport"
          content="width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <title>Demo for layout</title>
    <link href="https://cdn.bootcss.com/twitter-bootstrap/4.3.1/css/bootstrap.min.css" rel="stylesheet">
</head>
<body>
<?php $this->include('layouts/default/header') ?>

<div class="container">
    <!-- Content here -->
    <div id="page-content" style="padding: 15px 0;">{_CONTENT_}</div>
    <?php $this->include('layouts/default/footer') ?>
</div>
</body>
</html>
