.PHONY: all clone docker iosevka

all: clone docker iosevka nerd-font

clone:
	@if [ ! -d "Iosevka" ]; then \
		git clone --depth 1 https://github.com/be5invis/Iosevka.git; \
	fi
	@if [ ! -d "nerd-fonts" ]; then \
        git clone --depth 1 https://github.com/ryanoasis/nerd-fonts; \
    fi

docker:
	docker build -t=nerdfonts/patcher nerd-fonts
	docker build -t=fontcc Iosevka/docker

iosevka:
	docker run -it --rm -v ${PWD}:/work fontcc contents::iosevka-custom contents::iosevka-custom-term 

nerd-font:
	docker run --rm -v ./dist/iosevka-custom-term/TTF-Unhinted/:/in:Z -v ./dist:/out:Z nerdfonts/patcher --careful --complete
	docker run --rm -v ./dist/iosevka-custom/TTF-Unhinted/:/in:Z -v ./dist:/out:Z nerdfonts/patcher --careful --complete