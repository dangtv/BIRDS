## Usage

### Installation for jekyll

```bash
sudo gem install bundler
bundle install
```

### Generating webpage

Go to this folder `cd webpage`

Run `bundle exec jekyll build -d ../docs`

Check the built webpage: `bundle exec htmlproofer ../docs --check-html --check-sri --disable-external`