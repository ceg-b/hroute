#!/usr/bin/env python3

from OpenGL.GLU import *
from OpenGL.GL import *
from OpenGL.GLUT import *
from math import *

import sys

angle=0

def idle():
    glutPostRedisplay()


def wall():
    glBegin(GL_QUADS)
    glNormal(0,0,1)
    glVertex3f(0.0,  0.0, 0.0)
    glVertex3f(1.0,  0.0, 0.0)
    glVertex3f(1.0,  1.0, 0.0)
    glVertex3f(0.0,  1.0, 0.0)
    glEnd();

def box():
    glPushMatrix()
    glColor(1,0,0);
    wall()
    glColor(0,1,0);
    glRotatef(90,1,0,0);
    wall()
    glColor(0,0,1);
    glRotatef(90,0,1,0);
    wall()
    glTranslatef(0,0,1);
    wall()
    glPopMatrix()
    
def render_scene():
    global angle
    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)# Clear Screen And Depth Buffer
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity ()      # Reset The Modelview Matrix
    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LESS)
    glEnable(GL_LIGHTING)
    glFrustum(-1,1,-1,1,1,1000);
    glTranslatef(-.5,-.5,-10);
    glTranslatef(0,0,-10);
    glLight(GL_LIGHT0,GL_POSITION,[3*sin(angle),0,3*cos(angle),0])
    glDisable(GL_LIGHTING)
    glBegin(GL_POINTS)
    glVertex3f(3*sin(angle),0,3*cos(angle),0);
    glEnd()
    glEnable(GL_LIGHTING)
    glTranslatef(0,0,10);
    box();
    glutSwapBuffers()
    angle=angle+.0002
    return True





glutInit(sys.argv)
glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_ALPHA | GLUT_DEPTH)
glutInitWindowSize(640,480)
glutCreateWindow(b'window')

red   = ( 1.0, 0.1, 0.1, 1.0 )
white = ( 1.0, 1.0, 1.0, 1.0 )
glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE,  red  )
glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, white)
glMaterialf( GL_FRONT_AND_BACK, GL_SHININESS, 60.0)
glEnable(GL_LIGHT0)
glPointSize(5)
glutIdleFunc(idle)
glutDisplayFunc(render_scene)


glutMainLoop()
