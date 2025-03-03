#ifndef OPENGLRENDERING_HPP
#define OPENGLRENDERING_HPP

#include <string>
#include <SDL.h>
#include <SDL_opengl.h>

bool loadOpenGLRendering();
void unloadOpenGLRendering();
bool loadShaderProgram(std::string glslFilePath);
void renderSDLOpenGLBackBuffer(SDL_Window* window, SDL_Texture* backBuffer);

#endif // OPENGLRENDERING_HPP
