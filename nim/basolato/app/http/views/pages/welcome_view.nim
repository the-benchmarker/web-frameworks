import basolato/view
import ../layouts/application_view


proc impl(name:string):Component =
  style "css", style:"""
    <style>
      body {
        background-color: black;
      }

      article {
        margin: 16px;
      }

      .title {
        color: goldenrod;
        text-align: center;
      }

      .topImage {
        background-color: gray;
        text-align: center;
      }

      .goldFont {
        color: goldenrod;
      }

      .whiteFont {
        color: silver;
      }

      .ulLink li {
        margin: 8px;
      }

      .ulLink li a {
        color: skyblue;
      }

      .architecture {
        padding: 10px
      }

      .architecture h2 {
        color: goldenrod;
      }

      .components {
        display:flex;
      }

      .discription {
        width: 50vw;
      }

      .discription h3 {
        color: goldenrod;
      }

      .discription p {
        color: white;
      }

      .sourceCode {
        width: 50vw
      }

      .sourceCode p {
        color: white;
        margin-bottom: 0;
      }

      .sourceCode pre {
        margin-top: 0;
      }
    </style>
  """

  tmpli html"""
    $(style)
    <link rel="stylesheet" href="http://cdn.jsdelivr.net/gh/highlightjs/cdn-release@9.17.1/build/styles/dracula.min.css">
    <script src="http://cdn.jsdelivr.net/gh/highlightjs/cdn-release@9.17.1/build/highlight.min.js"></script>
    <article>
      <section>
        <h1 class="$(style.element("title"))">Nim $name is successfully running!!!</h1>
        <div class="$(style.element("topImage"))">
          <img
            src="/basolato.svg"
            alt="nim-logo"
            style="height: 40vh"
          >
        </div>
      </section>
    </article>
    <article>
      <section>
        <h2 class="$(style.element("goldFont"))">
          Full-stack Web Framewrok for Nim
        </h2>
        <p class="$(style.element("whiteFont"))">
          <i>—utilitas, firmitas et venustas (utility, strength and beauty)— by De architectura / Marcus Vitruvius Pollio</i>
        </p>
        <div class="$(style.element("whiteFont"))">
          <ul>
            <li>Easy syntax as Python thanks to Nim</li>
            <li>Develop as easy as Ruby on Rails</li>
            <li>Stably structure as Symfony(PHP)</li>
            <li>Including easy query builder as Laravel(PHP)</li>
            <li>Run fast and light as Go and Rust</li>
            <li>This is the fastest full-stack web framework in the world</li>
          </ul>
        </div>
      </section>
    </article>
  """

proc welcomeView*(name:string):string =
  let title = "Welcome Basolato"
  return $applicationView(title, impl(name))
