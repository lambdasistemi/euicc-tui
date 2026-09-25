# zbarimg alone: no camera, no X, no Qt/GTK, and an ImageMagick that
# reads only what the QR picker offers (PNG, JPEG, WebP; GIF and BMP
# are built in). The release closure shrinks by hundreds of MiB.
{ pkgs }:
let
  imagemagick = pkgs.imagemagick.override {
    bzip2Support = false;
    libX11Support = false;
    libXtSupport = false;
    fontconfigSupport = false;
    freetypeSupport = false;
    djvulibreSupport = false;
    lcms2Support = false;
    openexrSupport = false;
    libjxlSupport = false;
    liblqr1Support = false;
    libraqmSupport = false;
    librawSupport = false;
    librsvgSupport = false;
    libtiffSupport = false;
    libultrahdrSupport = false;
    libxml2Support = false;
    openjpegSupport = false;
    libheifSupport = false;
    fftwSupport = false;
  };
in
pkgs.zbar.override {
  enableVideo = false;
  withXorg = false;
  imagemagickBig = imagemagick;
}
