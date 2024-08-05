import requests
import matplotlib
import matplotlib.pyplot as plt
import re
from fontTools import ttLib
from tempfile import NamedTemporaryFile


def download_googlefont(font: str = "Heebo") -> None:
    """
    Download a font from Google Fonts and save it in the `fonts` folder.

    This code is modified from `Opinionated`
    (https://github.com/MNoichl/opinionated), which itself is borrowed from
    https://github.com/TutteInstitute/datamapplot.

    Args:
        font (str, optional): The name of the font to download from Google
            Fonts. Defaults to `Heebo`.
    """

    # Replace spaces with '+' to format the font name for the API URL
    api_fontname = font.replace(" ", "+")
    # Retrieve the CSS from Google Fonts API that contains the URLs for font
    # files
    api_response = requests.get(
        f"https://fonts.googleapis.com/css?family={api_fontname}:black,"
        "bold,regular,light"
    )
    # Extract font file URLs from the response content
    font_urls = re.findall(r"https?://[^\)]+", str(api_response.content))

    # Download and process each font file found
    for font_url in font_urls:
        # Download the font file
        font_data = requests.get(font_url)
        # Create a temporary file to save the downloaded font
        with NamedTemporaryFile(delete=False, suffix=".ttf") as f:
            f.write(font_data.content)
            # Ensure the file is written and closed properly
            f.close()

            # Load the font using fontTools library
            font = ttLib.TTFont(f.name)
            # Retrieve the font family name from the font's metadata
            font_family_name = font["name"].getDebugName(1)
            # Add the font to matplotlib's font manager for future use
            matplotlib.font_manager.fontManager.addfont(f.name)
            print(f"Added new font as {font_family_name}")


def set_style(
    style_path: str = "../style.mplstyle", font: str = "Heebo"
) -> None:
    """
    Set the Matplotlib style and download the specified font from Google
    Fonts.

    Args:
        style_path (str, optional): The path to the Matplotlib style file.
            Defaults to `../style.mplstyle`.
        font (str, optional): The name of the font to download from Google
            Fonts. Defaults to `Heebo`.
    """

    # Check whether matplotlib already has the font
    font_names = [f.name for f in matplotlib.font_manager.fontManager.ttflist]
    if font in font_names:
        print(f"Font {font} already available in Matplotlib.")
    else:
        download_googlefont(font)

    # Read the original style file and replace the font.family line with the
    # new font
    with open(style_path, "r") as f:
        style_lines = f.readlines()

    new_style_lines = [
        (
            line.replace("font.family: sans-serif", f"font.family: {font}")
            if line.startswith("font.family")
            else line
        )
        for line in style_lines
    ]

    # Use a temporary style file with updated font family
    with open("temp_style.mplstyle", "w") as f:
        f.writelines(new_style_lines)

    plt.style.use("temp_style.mplstyle")
    print(f"Matplotlib style set to: {style_path} with font {font}")