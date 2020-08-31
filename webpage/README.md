## Usage

### Installation for jekyll
Need to install ruby first.
For macosx:
```bash
brew install ruby
echo 'export PATH="/usr/local/opt/ruby/bin:$PATH"' >> ~/.bash_profile
echo 'export PATH=$HOME/gems/bin:$PATH' >> ~/.bash_profile
```

Then go to this folder (`cd webpage`) and run:
```bash
sudo gem install bundler
bundle install
```

### Generating the web pages

Go to this folder (`cd webpage`)

Run `bundle exec jekyll build -d ../docs`

Check the built webpage: `bundle exec htmlproofer ../docs --check-html --check-sri --disable-external`