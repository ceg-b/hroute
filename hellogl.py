#!/usr/bin/env python3

from OpenGL.GLU import *
from OpenGL.GL import *
from OpenGL.GLUT import *
from math import *

import sys
from PIL import Image
import os
import numpy as numpy

path='flower_photos/roses/'
angle=0
ang=85;
L=2
P=.5
counter=0
textures=[]
im_num=4
coordinate_system=False
draw_normal=False

def idle():
    glutPostRedisplay()

def light_on():
#    glEnable(GL_LIGHTING)
    pass

def light_off():
    glDisable(GL_LIGHTING)
    pass;

def mkTex(n,texture_to_kill=None):

    if not n:
        n=0
    for dirname, dirnames, filenames in os.walk(path):
        pass

    n=n %len(filenames)

    if texture_to_kill:
        print(texture_to_kill)
        glDeleteTextures([texture_to_kill]);

    image = Image.open(path+filenames[n])
    print('opened file: size=', image.size, 'format=', image.format)
    imageData = numpy.array(list(image.getdata()), numpy.uint8)

    textureID = glGenTextures(1)
    glPixelStorei(GL_UNPACK_ALIGNMENT, 4)
    glBindTexture(GL_TEXTURE_2D, textureID)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, 0)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, image.size[0], image.size[1],
                 0, GL_RGB, GL_UNSIGNED_BYTE, imageData)

    image.close()
    return textureID

def wall():
    glColor(1,1,1)
    glBegin(GL_QUADS)
    glTexCoord2f(1.0, 1.0)
    glVertex3f(0.0,  0.0, 0.0)
    glTexCoord2f(0.0, 1.0)
    glVertex3f(1.0,  0.0, 0.0)
    glTexCoord2f(0.0, 0.0)
    glVertex3f(1.0,  1.0, 0.0)
    glTexCoord2f(1.0, 0.0)
    glVertex3f(0.0,  1.0, 0.0)
    glEnd();

    if draw_normal:
        light_off()
        glBegin(GL_LINES)
        glColor(1,1,1)
        glVertex3f(0.5,  0.5, 0.0)
        glVertex3f(0.5,  0.5, -0.25)
        glEnd()
        light_on()


def wall1():
    glColor(1,1,1)
    chops = [ l for l in map(lambda x:0.1*x,range(10))]
    dl=0.1
    glBegin(GL_QUADS)
    for xx in chops:
        for yy in chops:
              glVertex3f(xx,  yy, 0.0)
              glVertex3f(xx+dl,  yy, 0.0)
              glVertex3f(xx+dl,  yy+dl, 0.0)
              glVertex3f(xx,  yy+dl, 0.0)
    glEnd();
    light_off()
    glBegin(GL_LINES)
    glColor(1,1,1)
    glVertex3f(0.5,  0.5, 0.0)
    glVertex3f(0.5,  0.5, -0.25)
    glEnd()
    light_on()

    
def box():
    global textures
    glPushMatrix()
    glBindTexture(GL_TEXTURE_2D, textures[0])
    glNormal(0,0,-1)
    wall()
    glTranslatef(0,0,1);
    glBindTexture(GL_TEXTURE_2D, textures[2])
    wall()
    glNormal(0,0,-1)
    glBindTexture(GL_TEXTURE_2D, textures[1])
    glRotatef(90,0,1,0);
    wall()
    glTranslatef(0,0,1);
    glBindTexture(GL_TEXTURE_2D, textures[3])
    wall()
    glPopMatrix()
    
def render_scene():
    global angle,ang,counter,textures,im_num
    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)# Clear Screen And Depth Buffer
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity ()      # Reset The Modelview Matrix
    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LESS)
    light_on()
    glFrustum(-1,1,-1,1,.9,100);
    glTranslatef(0,0,-1.7);
    glRotatef(ang,0,1,0)
    glTranslatef(-.5,-.5,-.5);
    glLight(GL_LIGHT0,GL_DIFFUSE,[1,1,1,0])
    glLightf(GL_LIGHT0, GL_CONSTANT_ATTENUATION, 0.1)
    glLightf(GL_LIGHT0, GL_LINEAR_ATTENUATION, 0.05)
    glLight(GL_LIGHT0,GL_POSITION,[P*sin(angle),0,P*cos(angle),0])
    light_off()
    glBegin(GL_POINTS)
    glVertex3f(P*sin(angle),0,P*cos(angle),0);
    glEnd()
    light_on()
    box();

# coordinate system
    if coordinate_system:
        light_off()
        glBegin(GL_LINES)
        glColor(1,0,0)
        glVertex3f(0.0,  0.0, 0.0)
        glVertex3f(L,  0.0, 0.0)
        glColor(1,1,1)
        glVertex3f(0.0,  0.0, 0.0)
        glVertex3f(0.0,  L, 0.0)
        glColor(1,1,0)
        glVertex3f(0.0,  0.0, 0.0)
        glVertex3f(0.0,  0.0, L)
        glEnd()
        light_on()

# eof    
    glutSwapBuffers()
    angle=angle+.0
    ang=ang+.3
    counter=counter+1;

    if (counter%200==0):
        im_num=im_num+1
        tmp=mkTex(im_num,textures[0])
        textures.append(tmp)
        textures=textures[1:]
    
    return True





### main ###
if __name__ == "__main__":

    if sys.argv[1]:
        path=sys.argv[1]


glutInit(sys.argv)
glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_ALPHA | GLUT_DEPTH)
glutInitWindowSize(640,480)
glutCreateWindow(b'window')

red   = ( 1.0, 0.5, 1.0, 1.0 )
white = ( 1.0, 1.0, 1.0, 1.0 )
glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE,  red  )
glEnable(GL_LIGHT0)
glEnable(GL_NORMALIZE)
glEnable(GL_TEXTURE_2D)
glShadeModel(GL_SMOOTH)
#glEnable(GL_RESCALE_NORMAL)
#glEnable(GL_CULL_FACE);
textures=[mkTex(0),mkTex(1),mkTex(2),mkTex(3)]
glPointSize(5)
glutIdleFunc(idle)
glutDisplayFunc(render_scene)


glutMainLoop()
