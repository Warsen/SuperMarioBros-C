#include <cstdio>
#include <iostream>
#include <string>
#include <fstream>
#include <streambuf>

#include <SDL.h>
#include <SDL_opengl.h>

#include "Emulation/Controller.hpp"
#include "SMB/SMBEngine.hpp"
#include "Util/Video.hpp"

#include "Configuration.hpp"
#include "Constants.hpp"

PFNGLCREATESHADERPROC glCreateShader;
PFNGLSHADERSOURCEPROC glShaderSource;
PFNGLCOMPILESHADERPROC glCompileShader;
PFNGLGETSHADERIVPROC glGetShaderiv;
PFNGLGETSHADERINFOLOGPROC glGetShaderInfoLog;
PFNGLDELETESHADERPROC glDeleteShader;
PFNGLATTACHSHADERPROC glAttachShader;
PFNGLCREATEPROGRAMPROC glCreateProgram;
PFNGLLINKPROGRAMPROC glLinkProgram;
PFNGLVALIDATEPROGRAMPROC glValidateProgram;
PFNGLGETPROGRAMIVPROC glGetProgramiv;
PFNGLGETPROGRAMINFOLOGPROC glGetProgramInfoLog;
PFNGLUSEPROGRAMPROC glUseProgram;
PFNGLGETUNIFORMLOCATIONPROC glGetUniformLocation;
PFNGLUNIFORM2FPROC glUniform2f;
PFNGLUNIFORM2FVPROC glUniform2fv;
PFNGLUNIFORM4FPROC glUniform4f;
PFNGLUNIFORM1IPROC glUniform1i;
PFNGLGETATTRIBLOCATIONPROC glGetAttribLocation;
PFNGLENABLEVERTEXATTRIBARRAYPROC glEnableVertexAttribArray;
PFNGLDISABLEVERTEXATTRIBARRAYPROC glDisableVertexAttribArray;
PFNGLBINDBUFFERPROC glBindBuffer;
PFNGLVERTEXATTRIBPOINTERPROC glVertexAttribPointer;
PFNGLUNIFORMMATRIX4FVPROC glUniformMatrix4fv;
PFNGLGENBUFFERSPROC glGenBuffers;
PFNGLBUFFERDATAPROC glBufferData;
PFNGLDELETEBUFFERSPROC glDeleteBuffers;

static GLuint VertexCoordLocation;
static GLuint ColorLocation;
static GLuint TexCoordLocation;
static GLint MVPMatrixLocation;
static GLint TextureSizeLocation;

// Discovery: If you don't do this with *2, the scale2x algorithm won't appear to do anything.
static float textureSizeData[2] = { RENDER_WIDTH*2, RENDER_HEIGHT*2 };

// Vertex Data
// Discovery: Looks like it needs an ortho matrix because the texture can not display in anything but the upper right
// quadrant due to some coordinate calculations used in the shader that uses the vertex coordinates for tex coordinates.
static GLfloat quadVertices[16] = {
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

/// <summary>
/// Initialize OpenGL extensions obtained from SDL2.
/// </summary>
/// <returns>returns True if the extensions could be loaded; otherwise False.</returns>
bool initGLExtensions()
{
	glCreateShader = (PFNGLCREATESHADERPROC)SDL_GL_GetProcAddress("glCreateShader");
	glShaderSource = (PFNGLSHADERSOURCEPROC)SDL_GL_GetProcAddress("glShaderSource");
	glCompileShader = (PFNGLCOMPILESHADERPROC)SDL_GL_GetProcAddress("glCompileShader");
	glGetShaderiv = (PFNGLGETSHADERIVPROC)SDL_GL_GetProcAddress("glGetShaderiv");
	glGetShaderInfoLog = (PFNGLGETSHADERINFOLOGPROC)SDL_GL_GetProcAddress("glGetShaderInfoLog");
	glDeleteShader = (PFNGLDELETESHADERPROC)SDL_GL_GetProcAddress("glDeleteShader");
	glAttachShader = (PFNGLATTACHSHADERPROC)SDL_GL_GetProcAddress("glAttachShader");
	glCreateProgram = (PFNGLCREATEPROGRAMPROC)SDL_GL_GetProcAddress("glCreateProgram");
	glLinkProgram = (PFNGLLINKPROGRAMPROC)SDL_GL_GetProcAddress("glLinkProgram");
	glValidateProgram = (PFNGLVALIDATEPROGRAMPROC)SDL_GL_GetProcAddress("glValidateProgram");
	glGetProgramiv = (PFNGLGETPROGRAMIVPROC)SDL_GL_GetProcAddress("glGetProgramiv");
	glGetProgramInfoLog = (PFNGLGETPROGRAMINFOLOGPROC)SDL_GL_GetProcAddress("glGetProgramInfoLog");
	glUseProgram = (PFNGLUSEPROGRAMPROC)SDL_GL_GetProcAddress("glUseProgram");
	glGetUniformLocation = (PFNGLGETUNIFORMLOCATIONPROC)SDL_GL_GetProcAddress("glGetUniformLocation");
	glUniform2f = (PFNGLUNIFORM2FPROC)SDL_GL_GetProcAddress("glUniform2f");
	glUniform2fv = (PFNGLUNIFORM2FVPROC)SDL_GL_GetProcAddress("glUniform2fv");
	glUniform4f = (PFNGLUNIFORM4FPROC)SDL_GL_GetProcAddress("glUniform4f");
	glUniform1i = (PFNGLUNIFORM1IPROC)SDL_GL_GetProcAddress("glUniform1i");

	glGenBuffers = (PFNGLGENBUFFERSPROC)SDL_GL_GetProcAddress("glGenBuffers");
	glBufferData = (PFNGLBUFFERDATAPROC)SDL_GL_GetProcAddress("glBufferData");
	glDeleteBuffers = (PFNGLDELETEBUFFERSPROC)SDL_GL_GetProcAddress("glDeleteBuffers");

	glGetAttribLocation = (PFNGLGETATTRIBLOCATIONPROC)SDL_GL_GetProcAddress("glGetAttribLocation");
	glEnableVertexAttribArray = (PFNGLENABLEVERTEXATTRIBARRAYPROC)SDL_GL_GetProcAddress("glEnableVertexAttribArray");
	glDisableVertexAttribArray = (PFNGLDISABLEVERTEXATTRIBARRAYPROC)SDL_GL_GetProcAddress("glDisableVertexAttribArray");
	glBindBuffer = (PFNGLBINDBUFFERPROC)SDL_GL_GetProcAddress("glBindBuffer");
	glVertexAttribPointer = (PFNGLVERTEXATTRIBPOINTERPROC)SDL_GL_GetProcAddress("glVertexAttribPointer");
	glUniformMatrix4fv = (PFNGLUNIFORMMATRIX4FVPROC)SDL_GL_GetProcAddress("glUniformMatrix4fv");

	return glCreateShader && glShaderSource && glCompileShader && glGetShaderiv && glGetShaderInfoLog &&
		glDeleteShader && glAttachShader && glCreateProgram && glLinkProgram && glValidateProgram &&
		glGetProgramiv && glGetProgramInfoLog && glUseProgram && glGetUniformLocation && glUniform2f &&
		glUniform4f && glUniform1i;
}

/// <summary>
/// Single function to compile either a vertex shader of fragment shader. Will use the value in shaderType to prepend
/// a define statement of either VERTEX or FRAGMENT and PARAMETER_UNIFORM in order to be compatible with libretro's
/// library of GLSL shaders (only the *.glsl files for now).
/// </summary>
/// <param name="source"></param>
/// <param name="shaderType"></param>
/// <returns></returns>
GLuint compileShader(const char* source, GLuint shaderType)
{
	GLuint result = glCreateShader(shaderType);

	// Prepend either VERTEX or FRAGMENT
	const char* sources[2];
	if (shaderType == GL_VERTEX_SHADER)
		sources[0] = "#version 130\n#define VERTEX\n#define PARAMETER_UNIFORM\n";
	else if (shaderType == GL_FRAGMENT_SHADER)
		sources[0] = "#version 130\n#define FRAGMENT\n#define PARAMETER_UNIFORM\n";
	sources[1] = source;

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
			GLchar* log = (GLchar*)malloc(logLength);
			glGetShaderInfoLog(result, logLength, &logLength, log);
			std::cout << "Shader Compile Log:" << log << std::endl;
			free(log);
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
GLuint compileProgram(const char* glslFile)
{
	GLuint programId = glCreateProgram();

	std::ifstream f(glslFile);
	std::string source((std::istreambuf_iterator<char>(f)), std::istreambuf_iterator<char>());
	GLuint vtxShaderId = compileShader(source.c_str(), GL_VERTEX_SHADER);

	f = std::ifstream(glslFile);
	source = std::string((std::istreambuf_iterator<char>(f)), std::istreambuf_iterator<char>());
	GLuint fragShaderId = compileShader(source.c_str(), GL_FRAGMENT_SHADER);

	if (vtxShaderId && fragShaderId)
	{
		// Associate shader with program
		glAttachShader(programId, vtxShaderId);
		glAttachShader(programId, fragShaderId);
		glLinkProgram(programId);
		glValidateProgram(programId);

		// Check the status of the compile/link
		GLint logLen;
		glGetProgramiv(programId, GL_INFO_LOG_LENGTH, &logLen);
		if (logLen > 0)
		{
			char* log = new char[logLen];
			glGetProgramInfoLog(programId, logLen, &logLen, log);
			std::cout << "Program Info Log: " << std::endl << log << std::endl;
			delete[] log;
		}
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

	// Run Once
	glUseProgram(programId); // Use immediately (!)

	// Set up the vertex shader input variables
	// Note: These should match the names and types declared in the shader code
	VertexCoordLocation = glGetAttribLocation(programId, "VertexCoord");
	ColorLocation = glGetAttribLocation(programId, "COLOR");
	TexCoordLocation = glGetAttribLocation(programId, "TexCoord");
	MVPMatrixLocation = glGetUniformLocation(programId, "MVPMatrix");
	TextureSizeLocation = glGetUniformLocation(programId, "TextureSize");

	// Create and bind a vertex buffer object (VBO) for the quad's vertices
	GLuint vbo;
	glGenBuffers(1, &vbo);
	glBindBuffer(GL_ARRAY_BUFFER, vbo);

	// Fill the VBO with the quad's vertices
	glBufferData(GL_ARRAY_BUFFER, sizeof(quadVertices), quadVertices, GL_STATIC_DRAW);

	// Delete the vertex buffer object (VBO)
	// You can't delete this while the program is running! This has to go in cleanup.
	//glDeleteBuffers(1, &vbo);

	return programId;
}

void presentBackBuffer(SDL_Renderer* renderer, SDL_Window* win, SDL_Texture* backBuffer, GLuint shaderProgram)
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
	glUniform2fv(TextureSizeLocation, 1, textureSizeData);

	// Render the quad
	glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

	// Disable the vertex attributes
	glDisableVertexAttribArray(VertexCoordLocation);
	glDisableVertexAttribArray(ColorLocation);
	glDisableVertexAttribArray(TexCoordLocation);

	/*
	// Without the use of a shader program:

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0, RENDER_WIDTH, RENDER_HEIGHT, 0, -1, 1);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	glBegin(GL_QUADS);
	glTexCoord2f(0, 0);
	glVertex2f(0.0f, 0.0f); // bottom left
	glTexCoord2f(1, 0);
	glVertex2f(RENDER_WIDTH, 0.0f); // bottom right
	glTexCoord2f(1, 1);
	glVertex2f(RENDER_WIDTH, RENDER_HEIGHT); // top right
	glTexCoord2f(0, 1);
	glVertex2f(0.0f, RENDER_HEIGHT); // top left
	glEnd();
	*/

	SDL_GL_SwapWindow(win);
}

// ------

uint8_t* romImage;
static SDL_Window* window;
static SDL_Renderer* renderer;
static SDL_Texture* texture;
static SDL_Texture* scanlineTexture;
static SDL_GameController* gameController;
static SMBEngine* smbEngine = nullptr;
static uint32_t renderBuffer[RENDER_WIDTH * RENDER_HEIGHT];
static GLuint programId;


/// <summary>
/// Load the Super Mario Bros. ROM image.
/// </summary>
/// <returns></returns>
static bool loadRomImage()
{
	FILE* file;
	errno_t err;
	if ((err = fopen_s(&file, Configuration::getRomFileName().c_str(), "r")) != 0)
	{
		std::cout << "Failed to open the file \"" << Configuration::getRomFileName() << "\". Exiting.\n";
		return false;
	}

	// Find the size of the file
	fseek(file, 0L, SEEK_END);
	size_t fileSize = ftell(file);
	fseek(file, 0L, SEEK_SET);

	// Read the entire file into a buffer
	romImage = new uint8_t[fileSize];
	fread(romImage, sizeof(uint8_t), fileSize, file);
	fclose(file);

	return true;
}

/// <summary>
/// SDL Audio callback function.
/// </summary>
/// <param name="userdata"></param>
/// <param name="buffer"></param>
/// <param name="len"></param>
static void audioCallback(void* userdata, uint8_t* buffer, int len)
{
	if (smbEngine != nullptr)
	{
		smbEngine->audioCallback(buffer, len);
	}
}



/// <summary>
/// Initialize libraries for use.
/// </summary>
/// <returns></returns>
static bool initialize()
{
	// Load the configuration
	Configuration::initialize(CONFIG_FILE_NAME);

	// Load the SMB ROM image
	if (!loadRomImage())
	{
		return false;
	}

	// Initialize SDL
	if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO | SDL_INIT_GAMECONTROLLER | SDL_VIDEO_OPENGL) < 0)
	{
		std::cout << "SDL_Init() failed during initialize(): " << SDL_GetError() << std::endl;
		return false;
	}

	// Create the window
	window = SDL_CreateWindow(APP_TITLE, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
		RENDER_WIDTH * Configuration::getRenderScale(), RENDER_HEIGHT * Configuration::getRenderScale(), 0);
	if (window == nullptr)
	{
		std::cout << "SDL_CreateWindow() failed during initialize(): " << SDL_GetError() << std::endl;
		return false;
	}

	// Setup the renderer and texture buffer
	SDL_SetHint(SDL_HINT_RENDER_DRIVER, "opengl");
	renderer = SDL_CreateRenderer(window, -1, (Configuration::getVsyncEnabled() ? SDL_RENDERER_PRESENTVSYNC : 0)
		| SDL_RENDERER_ACCELERATED | SDL_RENDERER_TARGETTEXTURE);
	if (renderer == nullptr)
	{
		std::cout << "SDL_CreateRenderer() failed during initialize(): " << SDL_GetError() << std::endl;
		return false;
	}

	// Get renderer info and make sure it's OpenGL, then compile the shader program.
	SDL_RendererInfo rendererInfo;
	SDL_GetRendererInfo(renderer, &rendererInfo);
	if (!strncmp(rendererInfo.name, "opengl", 6))
	{
		if (!initGLExtensions())
		{
			std::cout << "Couldn't init GL extensions!" << std::endl;
			SDL_Quit();
			exit(-1);
		}

		programId = compileProgram("scale2x.glsl"); // --- SHADER SELECTION ---
	}

	texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, RENDER_WIDTH, RENDER_HEIGHT);
	if (texture == nullptr)
	{
		std::cout << "SDL_CreateTexture() failed during initialize(): " << SDL_GetError() << std::endl;
		return false;
	}

	if (Configuration::getScanlinesEnabled())
	{
		scanlineTexture = generateScanlineTexture(renderer);
	}

	// Set up custom palette, if configured
	if (!Configuration::getPaletteFileName().empty())
	{
		const uint32_t* palette = loadPalette(Configuration::getPaletteFileName());
		if (palette)
		{
			paletteRGB = palette;
		}
	}

	if (Configuration::getAudioEnabled())
	{
		// Initialize audio
		SDL_AudioSpec desiredSpec;
		desiredSpec.freq = Configuration::getAudioFrequency();
		desiredSpec.format = AUDIO_S8;
		desiredSpec.channels = 1;
		desiredSpec.samples = 2048;
		desiredSpec.callback = audioCallback;
		desiredSpec.userdata = NULL;

		SDL_AudioSpec obtainedSpec;
		SDL_OpenAudio(&desiredSpec, &obtainedSpec);

		// Start playing audio
		SDL_PauseAudio(0);
	}

	return true;
}

/// <summary>
/// Shutdown libraries for exit.
/// </summary>
static void shutdown()
{
	if (gameController)
		SDL_GameControllerClose(gameController);

	SDL_CloseAudio();

	SDL_DestroyTexture(scanlineTexture);
	SDL_DestroyTexture(texture);
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);

	SDL_Quit();
}

/// <summary>
/// Main loop of the program runs the game program: CPU, PPU, APU.
/// </summary>
static void mainLoop()
{
	SMBEngine engine(romImage);
	smbEngine = &engine;
	engine.reset();

	bool running = true;
	int progStartTime = SDL_GetTicks();
	int frame = 0;
	while (running)
	{
		SDL_Event event;
		while (SDL_PollEvent(&event))
		{
			switch (event.type)
			{
			case SDL_QUIT:
				running = false;
				break;
			case SDL_WINDOWEVENT:
				switch (event.window.event)
				{
				case SDL_WINDOWEVENT_CLOSE:
					running = false;
					break;
				}
				break;

			case SDL_CONTROLLERDEVICEADDED:
				if (!gameController && event.cdevice.which == 0)
				{
					gameController = SDL_GameControllerOpen(0);
				}
				break;

			case SDL_CONTROLLERDEVICEREMOVED:
				if (gameController && event.cdevice.which == 0)
				{
					SDL_GameControllerClose(gameController);
					gameController = 0;
				}
				break;

			default:
				break;
			}
		}

		const Uint8* keys = SDL_GetKeyboardState(NULL);
		Controller& controller1 = engine.getController1();
		controller1.setButtonState(BUTTON_A, keys[SDL_SCANCODE_X]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_A)));
		controller1.setButtonState(BUTTON_B, keys[SDL_SCANCODE_Z]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_X)));
		controller1.setButtonState(BUTTON_SELECT, keys[SDL_SCANCODE_BACKSPACE]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_BACK)));
		controller1.setButtonState(BUTTON_START, keys[SDL_SCANCODE_RETURN]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_START)));
		controller1.setButtonState(BUTTON_UP, keys[SDL_SCANCODE_UP]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_DPAD_UP)));
		controller1.setButtonState(BUTTON_DOWN, keys[SDL_SCANCODE_DOWN]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_DPAD_DOWN)));
		controller1.setButtonState(BUTTON_LEFT, keys[SDL_SCANCODE_LEFT]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_DPAD_LEFT)));
		controller1.setButtonState(BUTTON_RIGHT, keys[SDL_SCANCODE_RIGHT]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_DPAD_RIGHT)));

		Controller& controller2 = engine.getController2();
		controller2.setButtonState(BUTTON_A, keys[SDL_SCANCODE_X]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_A)));
		controller2.setButtonState(BUTTON_B, keys[SDL_SCANCODE_Z]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_X)));
		controller2.setButtonState(BUTTON_SELECT, keys[SDL_SCANCODE_BACKSPACE]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_BACK)));
		controller2.setButtonState(BUTTON_START, keys[SDL_SCANCODE_RETURN]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_START)));
		controller2.setButtonState(BUTTON_UP, keys[SDL_SCANCODE_UP]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_DPAD_UP)));
		controller2.setButtonState(BUTTON_DOWN, keys[SDL_SCANCODE_DOWN]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_DPAD_DOWN)));
		controller2.setButtonState(BUTTON_LEFT, keys[SDL_SCANCODE_LEFT]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_DPAD_LEFT)));
		controller2.setButtonState(BUTTON_RIGHT, keys[SDL_SCANCODE_RIGHT]
			|| (gameController && SDL_GameControllerGetButton(gameController, SDL_CONTROLLER_BUTTON_DPAD_RIGHT)));

		if (keys[SDL_SCANCODE_R])
		{
			// Reset
			engine.reset();
		}
		if (keys[SDL_SCANCODE_ESCAPE])
		{
			// quit
			running = false;
			break;
		}
		if (keys[SDL_SCANCODE_F])
		{
			SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN_DESKTOP);
		}

		engine.update();
		engine.render(renderBuffer);

		// Updates the texture with new information from renderBuffer.
		SDL_UpdateTexture(texture, NULL, renderBuffer, sizeof(uint32_t) * RENDER_WIDTH);

		// New function to use OpenGL for presenting the texture as the backbuffer
		presentBackBuffer(renderer, window, texture, programId);

		// Ensure that the framerate stays as close to the desired FPS as possible. If the frame was rendered faster,
		// then delay. If the frame was slower, reset time so that the game doesn't try to "catch up", going super-speed.
		int now = SDL_GetTicks();
		int delay = progStartTime + int(double(frame) * double(MS_PER_SEC) / double(Configuration::getFrameRate())) - now;
		if (delay > 0) 
		{
			SDL_Delay(delay);
		}
		else
		{
			frame = 0;
			progStartTime = now;
		}
		frame++;
	}
}

/// <summary>
/// Entry point to the program.
/// </summary>
/// <param name="argc"></param>
/// <param name="argv"></param>
/// <returns></returns>
int main(int argc, char** argv)
{
	if (!initialize())
	{
		std::cout << "Failed to initialize. Please check previous error messages for more information. The program will now exit.\n";
		return -1;
	}

	mainLoop();

	shutdown();

	return 0;
}
