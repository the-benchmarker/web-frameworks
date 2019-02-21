<!DOCTYPE html>
<html>
<head>
    <title>500</title>
    <link rel="stylesheet" href="{{ asset('css/silver.css') }}">
</head>
<body>
#if($debug)
<div style='clear: both; font-size: 14px; font-family: Helvetica; color: #000;'>
    Current branch: <span style='color:#000; font-weight: bold; text-transform: uppercase;'>{{ ucfirst($_branch_) }}</span>
</div>
#endif
<header>
    <h1>500</h1>
    <h2>Server error</h2>
</header>
<div class="container">
    #if($debug)
        <div class="error-msg">
            <h5> {{ $message }}</h5>
        </div>
        <div><b>Error!</b>  {{ $file ?: $class }} <b>on line:{{ $line }}</b></div>
        <pre class="text-left"><code>{{ $code_around }}</code></pre>
        #foreach($back_trace as $bt)
            #if(isset($bt['file']))
                <div class="error-line">
                    <div class="col11" style="overflow: hidden;">
                        {{{ $bt['file'] }}}
                    </div>
                    <div class="col1">
                        <div class="put-left">on line:{{ $bt['line'] }}</div>
                    </div>
                </div>
            #endif
        #endforeach
    #else
         <h5></h5>
    #endif

</div>
<script src="{{ asset('js/code.js') }}"></script>

</body>
</html>




