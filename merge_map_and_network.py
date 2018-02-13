import glob

from PIL import Image


def remove_prefix(text, prefix):
    return text[text.startswith(prefix) and len(prefix):-4]


network_frames = glob.glob('./frames/network/*')
network_prefix = './frames/network\\network_'

network_frame_numbers = []
for i in range(len(network_frames)):
    network_frame_numbers.append(remove_prefix(network_frames[i], network_prefix))

for i in range(150):
    output_frame = './frames/movie_frames/movie_' + '{0:03d}'.format(i + 1) + '.png'
    map_frame = i % 120
    map_frame = './frames/merged_map/merged_' + '{0:03d}'.format(map_frame + 1) + '.png'
    background = Image.open(map_frame).convert('RGBA')

    if '{0:03d}'.format(i + 1) in network_frame_numbers:
        network_frame = './frames/network/network_' + '{0:03d}'.format(i + 1) + '.png'
        foreground = Image.open(network_frame).convert('RGBA')
        Image.alpha_composite(background, foreground).save(output_frame)

    else:
        background.save(output_frame)
