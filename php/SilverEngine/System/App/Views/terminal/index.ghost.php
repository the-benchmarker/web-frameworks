<!DOCTYPE html>
<html>
    <head>
	<title>Terminal power!</title>

	<style>
	 html {
	     background-color: #1d1f20;
	 }
	 body {
	     margin: 0;
	     height: 100%;
	 }
	</style>

	<link rel="stylesheet" type="text/css" href="{{ asset('css/terminal.css') }}" />
	<script src="{{ asset('js/terminal/lib.js') }}"></script>
	<script src="{{ asset('js/terminal/args.js') }}"></script>
	<script src="{{ asset('js/terminal/input.js') }}"></script>
	<script src="{{ asset('js/terminal/terminal.js') }}"></script>
	<script src="{{ asset('js/terminal/program.js') }}"></script>
    </head>

    <body>
	<style>
	 #wrapper {
	     position: absolute;
	     width: 100%;
	     height: 100%;
	     overflow-x: hidden;
	 }
	</style>
	<div id="wrapper">
	    <div id="terminal" class="terminal"></div>
	</div>

	<script>
	 var terminal = new Terminal('#terminal', {
	     'user': '{{ isset($user) ? $user : "" }}',
	     'font': '18px',
	 });
	 terminal.setScroll('#wrapper');
	 terminal.baseURL = "{{ route('terminal') }}";

	 terminal.envHandler('font', function(value) {
	     if(/^\d+$/.test(value)) {
		 value += "px";
	     }
	     this.element.style.fontSize = value;
	 });
	 
	 terminal.loadManifest(function() {
	     /* Prompt for login */
	     if(!terminal.env.user) {
		 terminal.clear();
		 terminal.run('login');
	     }
	 });
	</script>
    </body>
</html>
