# Iosevka customizer: https://typeof.net/Iosevka/customizer
# Nerd fonts cheat sheet: https://www.nerdfonts.com/cheat-sheet

.PHONY: clone docker iosevka nerd-font-patch

all: clone docker iosevka nerd-font-patch

clone:
# Clone Iosevka font or fetch latest if previously cloned.
	@if [ ! -d "Iosevka" ]; then \
		git clone --depth 1 https://github.com/be5invis/Iosevka.git; \
	else \
		git -C Iosevka pull; \
	fi

# Clone nerd-fonts or fetch latest if previously cloned.
	@if [ ! -d "nerd-fonts" ]; then \
		git clone --depth 1 https://github.com/ryanoasis/nerd-fonts; \
	else \
		git -C nerd-fonts pull; \
	fi

docker:
	docker build -t=nerdfonts/patcher nerd-fonts
	docker build -t=fontcc Iosevka/docker

# Build Iosevka distribution
iosevka:
	docker run -it --rm -v ${PWD}:/work fontcc ttf-unhinted::iosevka-custom ttf-unhinted::iosevka-custom-term

nerd-font-patch:
	docker run --rm -v ./dist/iosevka-custom-term/TTF-Unhinted/:/in:Z -v ./dist:/out:Z nerdfonts/patcher --careful --complete
	docker run --rm -v ./dist/iosevka-custom/TTF-Unhinted/:/in:Z -v ./dist:/out:Z nerdfonts/patcher --careful --complete