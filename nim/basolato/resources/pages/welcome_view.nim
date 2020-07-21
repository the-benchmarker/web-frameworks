import basolato/view
import ../layouts/application

proc impl(name:string):string = tmpli html"""
<link rel="stylesheet" href="http://cdn.jsdelivr.net/gh/highlightjs/cdn-release@9.17.1/build/styles/dracula.min.css">
<script src="http://cdn.jsdelivr.net/gh/highlightjs/cdn-release@9.17.1/build/highlight.min.js"></script>
<style>
  body {
    background-color: black;
  }

  #title {
    color: goldenrod;
    text-align: center;
  }

  #topImage {
    background-color: gray;
    text-align: center;
  }

  .goldFont {
    color: goldenrod
  }

  .whiteFont {
    color: white
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
    color: goldenrod
  }

  .components {
    display:flex
  }

  .discription {
    width: 50vw
  }

  .discription h3 {
    color: goldenrod
  }

  .discription p {
    color: white
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
<article>
  <section>
    <h1 id="title">Nim $name is successfully running!!!</h1>
    <div id="topImage">
      <img
        src="/basolato.svg"
        alt="nim-logo"
        style="height: 40vh"
      >
    </div>
    <h2 class="goldFont">
      "Fullstack Web Framewrok for Nim"
    </h2>
    <p class="whiteFont">
      <i>—utilitas, firmitas et venustas (utility, strength and beauty)— by De architectura / Marcus Vitruvius Pollio</i>
    </p>
    <p class="whiteFont">
      Develop as easy as Ruby on Rais, Stably as Laravel, Run faster and lighter than every other full-skack web framework.
    </p>
  </section>
</article>
"""

proc welcomeView*(this:View, name:string):string =
  let title = "Welcome"
  return this.applicationView(title, impl(name))
