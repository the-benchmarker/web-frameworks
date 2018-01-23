class Home::Index < BrowserAction
  get "/" do
    render Lucky::WelcomePage
  end
end
