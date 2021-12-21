
import numpy as np


def extend_image_grid(image: np.ndarray, iterations):
    width, height = image.shape
    empty_cols = np.full((height, (iterations + 1) * 3), fill_value='.')
    empty_rows = np.full(((iterations + 1) * 3, width + (iterations + 1) * 6), fill_value='.')
    image = np.hstack([empty_cols, image, empty_cols])
    image = np.vstack([empty_rows, image, empty_rows])
    return image


def get_input_pixels(pixel, image):
    width, height = image.shape
    x, y = pixel
    surrounding = [(i, j) for i in range(x-1, x+2) for j in range(y-1, y+2)]
    pixels = []
    for px, py in surrounding:
        if 0 <= px < width and 0 <= py < height:
            pixels.append(image[px][py])
        else:
            pixels.append(None)
    return pixels


def correct_pixels(pixels, iteration, algorithm):
    corrected_pixels = []
    for pixel in pixels:
        if pixel:
            corrected_pixels.append(pixel)
            continue
        empty_pixel = algorithm[0]
        if iteration % 2 == 1:
            corrected_pixels.append('.')
        else:
            corrected_pixels.append(empty_pixel)
    return corrected_pixels
                

def pixels_to_index(pixels):
    bytes = ''.join(list(map(lambda c: '1' if c == "#" else '0', pixels)))
    return int(bytes, 2)


def enhance(image, algorithm, iteration):
    new_image = np.full(image.shape, fill_value='.')
    for pixel, _ in np.ndenumerate(image):
        input_pixels = get_input_pixels(pixel, image)
        corrected_pixels = correct_pixels(input_pixels, iteration, algorithm)
        index = pixels_to_index(corrected_pixels)
        enhanced_pixel = algorithm[index]
        new_image[pixel[0]][pixel[1]] = enhanced_pixel
    return new_image


def show_image(image):
    print('\n'.join(''.join(x for x in y) for y in image))


def count_lit_pixels(image):
    lit_count = 0
    for _, pixel in np.ndenumerate(image):
        if pixel == "#":
            lit_count += 1
    return lit_count


def main():
    with open("data.txt") as f:
        algorithm = f.readline().replace("\n", "")
        next(f)
        image = [l.replace("\n", "") for l in f.readlines()]
    image = np.array([list(r) for r in image])
    iterations = 50
    image = extend_image_grid(image, iterations=iterations)
    # show_image(image)
    iteration = 0
    for _ in range(iterations):
        iteration += 1
        image = enhance(image, algorithm, iteration)
        # show_image(image)
    print(count_lit_pixels(image))

if __name__ == "__main__":
    main()
