import os

from PIL import Image

lines_dir = './frames/maplines'
tiles_dir = './frames/maptiles'
merged_dir = './frames/merged_map'

num_frames = len(os.listdir(lines_dir))
lines_prefix = lines_dir + '/maplines'
tiles_prefix = tiles_dir + '/maptiles'
merged_prefix = merged_dir + '/merged'

for frame in range(num_frames):
    line_frame = lines_prefix + '_{0:03d}'.format(frame + 1) + '.png'
    tile_frame = tiles_prefix + '_{0:03d}'.format(frame + 1) + '.png'
    output_frame = merged_prefix + '_{0:03d}'.format(frame + 1) + '.png'

    background = Image.open(tile_frame).convert('RGBA')
    foreground = Image.open(line_frame).convert('RGBA')

    Image.alpha_composite(background, foreground).save(output_frame)
