import pygame
import math
import sys
img_width = 800
img_height = 800

pygame.init()
screen = pygame.display.set_mode((800, 800))
surf = pygame.Surface((img_width, img_height))

y = 0
x = 0
for line in sys.stdin:
    if line == "endline\n":
        pygame.transform.scale(surf, (800, 800), screen)
        pygame.display.flip()
        x += 1
        y = 0
        print("line ", x)
        continue

    if line == "endimage\n":
        pygame.transform.scale(surf, (800, 800), screen)
        pygame.display.flip()
        break

    colors = map(float, line[:-1].split(" "))
    surf.set_at((x, y), (colors[0], colors[1], colors[2]))
    y += 1


running = True
while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False



    #draw pixels here
pygame.quit()