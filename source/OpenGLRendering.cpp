#include <cstdio>
#include <iostream>
#include <string>
#include <fstream>
#include <streambuf>
#include <sstream>
#include <array>
#include <filesystem>
#include <unordered_map>

#include "Constants.hpp"
#include "OpenGLRendering.hpp"

PFNGLCREATEPROGRAMPROC glCreateProgram;
PFNGLCREATESHADERPROC glCreateShader;
PFNGLSHADERSOURCEPROC glShaderSource;
PFNGLCOMPILESHADERPROC glCompileShader;
PFNGLATTACHSHADERPROC glAttachShader;
PFNGLDELETESHADERPROC glDeleteShader;
PFNGLLINKPROGRAMPROC glLinkProgram;
PFNGLVALIDATEPROGRAMPROC glValidateProgram;
PFNGLGETPROGRAMIVPROC glGetProgramiv;
PFNGLGETSHADERIVPROC glGetShaderiv;
PFNGLGETPROGRAMINFOLOGPROC glGetProgramInfoLog;
PFNGLGETSHADERINFOLOGPROC glGetShaderInfoLog;
PFNGLUSEPROGRAMPROC glUseProgram;
PFNGLGENBUFFERSPROC glGenBuffers;
PFNGLBINDBUFFERPROC glBindBuffer;
PFNGLBUFFERDATAPROC glBufferData;
PFNGLDELETEBUFFERSPROC glDeleteBuffers;
PFNGLGETATTRIBLOCATIONPROC glGetAttribLocation;
PFNGLGETUNIFORMLOCATIONPROC glGetUniformLocation;
PFNGLENABLEVERTEXATTRIBARRAYPROC glEnableVertexAttribArray;
PFNGLDISABLEVERTEXATTRIBARRAYPROC glDisableVertexAttribArray;
PFNGLVERTEXATTRIBPOINTERPROC glVertexAttribPointer;
PFNGLUNIFORMMATRIX4FVPROC glUniformMatrix4fv;
PFNGLUNIFORM2FVPROC glUniform2fv;

static GLuint VertexCoordLocation;
static GLuint ColorLocation;
static GLuint TexCoordLocation;
static GLint MVPMatrixLocation;
static GLint TextureSizeLocation;

static std::unordered_map<std::string, GLuint> glslProgramMap;
static GLuint vbo;

// Discovery: If you don't do this with *2, the scale2x algorithm won't appear to do anything.
static float TextureSizeData[2] = { RENDER_WIDTH * 2, RENDER_HEIGHT * 2 };

// Vertex Data
// Discovery: Looks like it needs an ortho matrix because the texture can not display in anything but the upper right
// quadrant due to some coordinate calculations used in the shader that uses the vertex coordinates for tex coordinates.
static GLfloat VertexData[16] = {
	0.0f, 0.0f, 0.0f, 1.0f,   // Vertex 1: bottom-left
	1.0f, 0.0f, 0.0f, 1.0f,   // Vertex 2: bottom-right
	0.0f, 1.0f, 0.0f, 1.0f,   // Vertex 3: top-left
	1.0f, 1.0f, 0.0f, 1.0f    // Vertex 4: top-right
};

// MVP Matrix
// Discovery: If you only use an identity matrix, the image will display upside down in the upper-right quadrant.
// It might be that the reason it is displaying upside down is because the texture is in fact upside down in SDL.
// Discovery: If you use an orthogonal matrix, the matrix needs to be flipped so that column 4 becomes row 4.
// This might be because the author of these shaders uses a different matrix convention than OpenGL does.
// Discovery: In order to flip the image upside down, I needed to invert the signs on column 2.
static GLfloat MVPMatrixData[16] = {
	2.0f, 0.0f, 0.0f, 0.0f,
	0.0f, -2.0f, 0.0f, 0.0f,
	0.0f, 0.0f, 1.0f, 0.0f,
	-1.0f, 1.0f, -1.0f, 1.0f
};

// Function Prototypes

bool initGLExtensions();
GLuint compileShader(const std::string& glslSource, GLuint shaderType);
bool processShaderProgram(const std::string& glslSource, GLuint& outProgramId);

/// <summary>
/// Initialize OpenGL extensions obtained from SDL2.
/// </summary>
/// <returns>returns True if the extensions could be loaded; otherwise False.</returns>
bool initGLExtensions()
{
	glCreateProgram = (PFNGLCREATEPROGRAMPROC)SDL_GL_GetProcAddress("glCreateProgram");
	glCreateShader = (PFNGLCREATESHADERPROC)SDL_GL_GetProcAddress("glCreateShader");
	glShaderSource = (PFNGLSHADERSOURCEPROC)SDL_GL_GetProcAddress("glShaderSource");
	glCompileShader = (PFNGLCOMPILESHADERPROC)SDL_GL_GetProcAddress("glCompileShader");
	glAttachShader = (PFNGLATTACHSHADERPROC)SDL_GL_GetProcAddress("glAttachShader");
	glDeleteShader = (PFNGLDELETESHADERPROC)SDL_GL_GetProcAddress("glDeleteShader");
	glLinkProgram = (PFNGLLINKPROGRAMPROC)SDL_GL_GetProcAddress("glLinkProgram");
	glValidateProgram = (PFNGLVALIDATEPROGRAMPROC)SDL_GL_GetProcAddress("glValidateProgram");
	glGetProgramiv = (PFNGLGETPROGRAMIVPROC)SDL_GL_GetProcAddress("glGetProgramiv");
	glGetShaderiv = (PFNGLGETSHADERIVPROC)SDL_GL_GetProcAddress("glGetShaderiv");
	glGetProgramInfoLog = (PFNGLGETPROGRAMINFOLOGPROC)SDL_GL_GetProcAddress("glGetProgramInfoLog");
	glGetShaderInfoLog = (PFNGLGETSHADERINFOLOGPROC)SDL_GL_GetProcAddress("glGetShaderInfoLog");
	glUseProgram = (PFNGLUSEPROGRAMPROC)SDL_GL_GetProcAddress("glUseProgram");
	glGenBuffers = (PFNGLGENBUFFERSPROC)SDL_GL_GetProcAddress("glGenBuffers");
	glBindBuffer = (PFNGLBINDBUFFERPROC)SDL_GL_GetProcAddress("glBindBuffer");
	glBufferData = (PFNGLBUFFERDATAPROC)SDL_GL_GetProcAddress("glBufferData");
	glDeleteBuffers = (PFNGLDELETEBUFFERSPROC)SDL_GL_GetProcAddress("glDeleteBuffers");
	glGetAttribLocation = (PFNGLGETATTRIBLOCATIONPROC)SDL_GL_GetProcAddress("glGetAttribLocation");
	glGetUniformLocation = (PFNGLGETUNIFORMLOCATIONPROC)SDL_GL_GetProcAddress("glGetUniformLocation");
	glEnableVertexAttribArray = (PFNGLENABLEVERTEXATTRIBARRAYPROC)SDL_GL_GetProcAddress("glEnableVertexAttribArray");
	glDisableVertexAttribArray = (PFNGLDISABLEVERTEXATTRIBARRAYPROC)SDL_GL_GetProcAddress("glDisableVertexAttribArray");
	glVertexAttribPointer = (PFNGLVERTEXATTRIBPOINTERPROC)SDL_GL_GetProcAddress("glVertexAttribPointer");
	glUniformMatrix4fv = (PFNGLUNIFORMMATRIX4FVPROC)SDL_GL_GetProcAddress("glUniformMatrix4fv");
	glUniform2fv = (PFNGLUNIFORM2FVPROC)SDL_GL_GetProcAddress("glUniform2fv");

	return glCreateProgram && glCreateShader && glShaderSource && glCompileShader && glAttachShader && glDeleteShader
		&& glLinkProgram && glValidateProgram && glGetProgramiv && glGetShaderiv && glGetProgramInfoLog
		&& glGetShaderInfoLog && glUseProgram && glGenBuffers && glBindBuffer && glBufferData && glDeleteBuffers
		&& glGetAttribLocation && glGetUniformLocation && glEnableVertexAttribArray && glDisableVertexAttribArray
		&& glVertexAttribPointer && glUniformMatrix4fv && glUniform2fv;
}

/// <summary>
/// Single function to compile either a vertex shader of fragment shader. Will use the value in shaderType to prepend
/// a define statement of either VERTEX or FRAGMENT and PARAMETER_UNIFORM in order to be compatible with libretro's
/// library of GLSL shaders (only the *.glsl files for now).
/// </summary>
/// <param name="source"></param>
/// <param name="shaderType"></param>
/// <returns></returns>
GLuint compileShader(const std::string& glslSource, GLuint shaderType)
{
	GLuint result = glCreateShader(shaderType);

	// Prepend either VERTEX or FRAGMENT depending on shaderType
	const char* sources[2];
	if (shaderType == GL_VERTEX_SHADER)
		sources[0] = "#define VERTEX\n";
	else if (shaderType == GL_FRAGMENT_SHADER)
		sources[0] = "#define FRAGMENT\n";
	sources[1] = glslSource.c_str();

	// Set shader source and compile
	glShaderSource(result, 2, sources, NULL);
	glCompileShader(result);

	// Check if compilation was successful
	GLint shaderCompiled = GL_FALSE;
	glGetShaderiv(result, GL_COMPILE_STATUS, &shaderCompiled);
	if (shaderCompiled != GL_TRUE)
	{
		std::cout << "Compilation Error: " << result << "!" << std::endl;
		GLint logLength;
		glGetShaderiv(result, GL_INFO_LOG_LENGTH, &logLength);
		if (logLength > 0)
		{
			char* log = new char[logLength];
			glGetShaderInfoLog(result, logLength, &logLength, log);
			std::cout << "Shader Compile Log:" << log << std::endl;
			delete[] log;
		}
		glDeleteShader(result);
		result = 0;
	}
	else
	{
		std::cout << "Shader Compiled Successfully. Id = " << result << std::endl;
	}

	return result;
}

/// <summary>
/// Compiles the requested GLSL program file which contains both Vertex and Fragment shaders.
/// </summary>
/// <param name="glslFile"></param>
/// <returns></returns>
bool processShaderProgram(const std::string& glslSource, GLuint& outProgramId)
{
	GLuint vtxShaderId = compileShader(glslSource, GL_VERTEX_SHADER);
	GLuint fragShaderId = compileShader(glslSource, GL_FRAGMENT_SHADER);

	if (vtxShaderId && fragShaderId)
	{
		// Associate compiled shaders with a shader program, link and validate the program.
		outProgramId = glCreateProgram();
		glAttachShader(outProgramId, vtxShaderId);
		glAttachShader(outProgramId, fragShaderId);
		glLinkProgram(outProgramId);
		glValidateProgram(outProgramId);
	}

	// Clean up shader memory because they are no longer needed after they are linked to the program.
	if (vtxShaderId)
	{
		glDeleteShader(vtxShaderId);
	}
	if (fragShaderId)
	{
		glDeleteShader(fragShaderId);
	}

	// Success depends on whether both compiled successfully.
	return vtxShaderId && fragShaderId;
}

bool loadShaderProgram(std::string glslFilePath)
{
	// Normalize the file path
	glslFilePath = std::filesystem::path(glslFilePath).lexically_normal().string();

	// Check if the shader program is already cached. By specification, created program IDs are non-zero.
	if (glslProgramMap[glslFilePath] > 0)
	{
		// Use the cached shader program
		glUseProgram(glslProgramMap[glslFilePath]);

		// Set up the vertex shader input variables
		// Note: These should match the names and types declared in the shader code
		VertexCoordLocation = glGetAttribLocation(glslProgramMap[glslFilePath], "VertexCoord");
		ColorLocation = glGetAttribLocation(glslProgramMap[glslFilePath], "COLOR");
		TexCoordLocation = glGetAttribLocation(glslProgramMap[glslFilePath], "TexCoord");
		MVPMatrixLocation = glGetUniformLocation(glslProgramMap[glslFilePath], "MVPMatrix");
		TextureSizeLocation = glGetUniformLocation(glslProgramMap[glslFilePath], "TextureSize");
	}
	else
	{
		// Check if the file exists. If it does, compile and use it, otherwise print error.
		if (std::filesystem::exists(glslFilePath))
		{
			std::ifstream ifs = std::ifstream(glslFilePath);
			if (ifs.good())
			{
				// Read the entire file's contents to a string and then process it to get a usable shader program.
				std::stringstream buffer;
				buffer << ifs.rdbuf();
				if (processShaderProgram(buffer.str(), glslProgramMap[glslFilePath]))
				{
					// Use the newly cached shader program
					glUseProgram(glslProgramMap[glslFilePath]);

					// Set up the vertex shader input variables
					// Note: These should match the names and types declared in the shader code
					VertexCoordLocation = glGetAttribLocation(glslProgramMap[glslFilePath], "VertexCoord");
					ColorLocation = glGetAttribLocation(glslProgramMap[glslFilePath], "COLOR");
					TexCoordLocation = glGetAttribLocation(glslProgramMap[glslFilePath], "TexCoord");
					MVPMatrixLocation = glGetUniformLocation(glslProgramMap[glslFilePath], "MVPMatrix");
					TextureSizeLocation = glGetUniformLocation(glslProgramMap[glslFilePath], "TextureSize");
				}
				else
				{
					std::cout << "Error: Unable to process file \"" << glslFilePath << "\"" << std::endl;
					return false;
				}
			}
			else
			{
				std::cout << "Error: Unable to read file \"" << glslFilePath << "\"" << std::endl;
				return false;
			}
		}
		else
		{
			std::cout << "Error: The file \"" << glslFilePath << "\" does not exist." << std::endl;
			return false;
		}
	}

	return true;
}

void renderSDLOpenGLBackBuffer(SDL_Window* window, SDL_Texture* backBuffer)
{
	// This binds the SDL texture 'backBuffer' into OpenGL for drawing.
	SDL_GL_BindTexture(backBuffer, NULL, NULL);

	// Enable the vertex attributes
	glEnableVertexAttribArray(VertexCoordLocation);
	glEnableVertexAttribArray(ColorLocation);
	glEnableVertexAttribArray(TexCoordLocation);

	// Set the vertex attribute pointers
	glVertexAttribPointer(VertexCoordLocation, 4, GL_FLOAT, GL_FALSE, 0, 0);
	glVertexAttribPointer(ColorLocation, 4, GL_FLOAT, GL_FALSE, 0, 0);
	glVertexAttribPointer(TexCoordLocation, 4, GL_FLOAT, GL_FALSE, 0, 0);

	// Set the uniform MVPMatrix in the shader program
	glUniformMatrix4fv(MVPMatrixLocation, 1, GL_FALSE, MVPMatrixData);

	// Set the texture size uniform
	glUniform2fv(TextureSizeLocation, 1, TextureSizeData);

	// Render the quad
	glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

	// Disable the vertex attributes
	glDisableVertexAttribArray(VertexCoordLocation);
	glDisableVertexAttribArray(ColorLocation);
	glDisableVertexAttribArray(TexCoordLocation);

	SDL_GL_SwapWindow(window);
}


bool loadOpenGLRendering()
{
	// Initialize OpenGL extensions
	if (!initGLExtensions())
	{
		std::cout << "Couldn't initialize all neccessary OpenGL extensions." << std::endl;
		return false;
	}

	if (!std::filesystem::exists("stock.glsl") || !loadShaderProgram("stock.glsl"))
	{
		std::cout << "Loading internal stock shader..." << std::endl;

		std::string stockShaderSource = "\
#if defined(VERTEX)\n\
attribute vec4 VertexCoord;\n\
attribute vec4 COLOR;\n\
attribute vec4 TexCoord;\n\
varying vec4 COL0;\n\
varying vec4 TEX0;\n\
uniform mat4 MVPMatrix;\n\
uniform int FrameDirection;\n\
uniform int FrameCount;\n\
uniform vec2 OutputSize;\n\
uniform vec2 TextureSize;\n\
uniform vec2 InputSize;\n\
void main()\n\
{\n\
	gl_Position = VertexCoord.x * MVPMatrix[0] + VertexCoord.y * MVPMatrix[1] + VertexCoord.z * MVPMatrix[2] + VertexCoord.w * MVPMatrix[3];\n\
	TEX0.xy = TexCoord.xy;\n\
}\n\
#elif defined(FRAGMENT)\n\
uniform int FrameDirection;\n\
uniform int FrameCount;\n\
uniform vec2 OutputSize;\n\
uniform vec2 TextureSize;\n\
uniform vec2 InputSize;\n\
uniform sampler2D Texture;\n\
varying vec4 TEX0;\n\
void main()\n\
{\n\
	gl_FragColor = texture2D(Texture, TEX0.xy);\n\
}\n\
#endif\n";

		processShaderProgram(stockShaderSource, glslProgramMap["stock.glsl"]);

		GLuint programId = glslProgramMap["stock.glsl"];

		// Use the newly cached shader program
		glUseProgram(programId);

		// Set up the vertex shader input variables
		// Note: These should match the names and types declared in the shader code
		VertexCoordLocation = glGetAttribLocation(programId, "VertexCoord");
		ColorLocation = glGetAttribLocation(programId, "COLOR");
		TexCoordLocation = glGetAttribLocation(programId, "TexCoord");
		MVPMatrixLocation = glGetUniformLocation(programId, "MVPMatrix");
		TextureSizeLocation = glGetUniformLocation(programId, "TextureSize");
	}

	// Create and bind a vertex buffer object (VBO) for the vertices
	glGenBuffers(1, &vbo);
	glBindBuffer(GL_ARRAY_BUFFER, vbo);
	glBufferData(GL_ARRAY_BUFFER, sizeof(VertexData), VertexData, GL_STATIC_DRAW);

	return true;
}

void unloadOpenGLRendering()
{
	// Delete the vertex buffer object (VBO)
	glDeleteBuffers(1, &vbo);
}
