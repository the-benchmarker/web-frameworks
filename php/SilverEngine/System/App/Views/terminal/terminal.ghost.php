<html>
    <head>
	<script src="http://ace.c9.io/build/src-min-noconflict/ace.js"></script>

        <title></title>
        <style>
            *
            {
                box-sizing: border-box;
            }

            body
            {
                font-family: courier new, monospace;
                color: #0f8;
                margin: 30px;
                font-size: 14px;
                height: 10vh;
                overflow: auto;
                background-color: #1d1f20;
                   overflow-y: auto;
                overflow-x: hidden;
            }

         #container {
	     position: relative;
                margin-bottom: 30px;
                overflow-y: hidden;
                overflow-x: hidden;
            }

            #terminal
            {
                white-space: pre-wrap;
                line-height: 1.4;
            }

            #cursor2
            {
                display: inline-block;
                height: 1.35em;
                width: 0.7em;
                vertical-align: middle;
                background-color: #fff;
            }

div.output {
    color: white;
}

            .invisible
            {
                opacity: 0;
            }

            a {
                color: inherit;
                text-decoration: underline;
            }
        </style>
        <script   src="https://code.jquery.com/jquery-2.2.4.min.js"   integrity="sha256-BbhdlvQf/xTY9gja0Dq3HiwQF8LaCRTXxZKRutelT44="   crossorigin="anonymous"></script>
        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css">
    </head>
    <body>
    <div id="container">
        <p id="terminal"></p>
    </div>
    <div id="visual"></div>

<!-- VISUAL programs -->
    <style>
     #visual {
	 position: absolute;
	 width: 100%;
	 height: 100%;
	 background-color: blue;
	 top: 0;
	 left: 0;
	 display: none;
     }
    </style>
    
    <script>
     var is_program_opened = false;
     function program_close() {
	 is_program_opened = false;
	 $('#visual').hide();
     }

     function program_toggle() {
	 is_program_opened = !is_program_opened;
	 $('#visual').toggle();
     }
     
     function program_launch(html) {
	 $('#visual').html(html);
	 is_program_opened = true;
	 $('#visual').show();
     }
    </script>
<!-- END OF VISUAL programs -->

                
    <script>
        var commands = {
            'clear': clearConsole,
            'test': function(args) {
                printOutput(JSON.stringify(args));
            },
	    'js': function(args) {
		printOutput(eval(args.join(' ')));
	    }
        };



        var cursor = $('#cursor2');
        var terminal = $('#terminal');
        var text = ["Welcome on SilverEngine terminal v1.0.0.\n$log in: ", ""];
var commandHistory = (function(){
    var history = localStorage.getItem('command-history');
    if(history)
        return JSON.parse(history);
})() || [];
        var lineY = 1;
        var index = 0;
        var historyIndex = commandHistory.length;

        setInterval(function() {
            cursor.toggleClass('invisible');
        }, 500);

        function clearConsole() {
            text = [];
            lineY = 0;
        }

        function printConsole(txt) {
            cursor.remove();
            terminal.html(text);
            terminal.append('<span id="cursor2" autofocus></span>');
            cursor = $('#cursor2');
            document.body.scrollTop = $('#container').height();
        }



        function printText(ttt) {
            text[lineY++] = ttt;
        }

function printOutput(ttt) {
    text[lineY++] = '<div class="output">' + ttt + '</div>';
}

        function processCommand(rawData) {
            var args = rawData.split(" ");
            var command = args[0];
            commandHistory[historyIndex++] = rawData;
            localStorage.setItem('command-history', JSON.stringify(commandHistory));
            args.shift();

            if(!args)
                args = [];

            printText(rawData);
            if(commands[command]) {
                commands[command](args);
            } else {
                var ret = JSON.parse(processRemote(command, args));
                printOutput(ret['output']);

                if(ret['visual']) {
                    program_launch(ret['visual']);
                }
            }
        }

        function processRemote(command, args) {
            var content = '';
            $.ajax({
                type: 'POST',
                url: '{{{ $TERMINAL_URL}}}',
                data: {
                    'command': command,
                    'args': args
                },
                async: false,
                success: function(data) {
                    content = data;
                }
            });
            return content;
        }

        function nextLine() {
            processCommand(text[lineY]);
            if (lineY != 0) {
                lineY++;
                text[lineY] = "\n";
            } else
                text[lineY] = "";

            text[lineY] += "$server@root: ";
            lineY++;
            text[lineY] = "";
            printConsole(text);
        }

        function erase(n) {
            text[lineY] = text[lineY].slice(0, -n);
            index--;
            printConsole(text);
        }

        $(document).ready(function() {
            printConsole(text)
        })

        $('html').on('keydown', function(e) {
            e = e || window.event;
            var keyCode = typeof e.which === "number" ? e.which : e.keyCode;

            // Backspace? Erase!
            if (keyCode === 8 && e.target.tagName !== "INPUT" && e.target.tagName !== "TEXTAREA") {
                e.preventDefault();
                if (index != 0)
                    erase(1);
            }
        });

     $(document).keydown(function(e) {
	 switch(e.which) {
	     case 38:
		 if(is_program_opened) return;
		 if(text[lineY]) {
		     commandHistory.unshift(text[lineY]);
		 }
		 text[lineY] = commandHistory.pop();
		 index = text[lineY].length;
		 printConsole(text);
		 return false;
	     case 40:
		 if(is_program_opened) return;
		 if(text[lineY]) {
		     commandHistory.push(text[lineY]);
		 }
		 text[lineY] = commandHistory.shift();
		 index = text[lineY].length;
		 printConsole(text);
		 return false;
	     case 27:
		 program_toggle();
	 }
     });

        $(document).keypress(function(e) {
	if(is_program_opened) return;
            // Make sure we get the right event
            e = e || window.event;
            var keyCode = typeof e.which === "number" ? e.which : e.keyCode;

            // Which key was pressed?
            switch (keyCode) {
                // ENTER
                case 13:
                    {
                    nextLine();
                    break;
                }
                default:
                {
                    var data = String.fromCharCode(keyCode);
                    if (data != undefined) {
                        var length = text[lineY].length;
                        text[lineY] += data;
                        index = index + (text[lineY].length - length);
                        printConsole(text);
                    }
                    break;
                }
            }
        });
    </script>
    </body>
</html>
