#include <fstream>

#include "Configuration.hpp"

/**
 * List of all supported configuration options.
 */
std::map<std::string, ConfigurationOption*> Configuration::configurationOptions = {
	{ "audio.enabled", new BasicConfigurationOption<bool>(audioEnabled) },
	{ "audio.frequency", new BasicConfigurationOption<int>(audioFrequency) },
	{ "game.frame_rate", new BasicConfigurationOption<int>(frameRate) },
	{ "video.palette_file", new BasicConfigurationOption<std::string>(paletteFileName) },
	{ "video.scale", new BasicConfigurationOption<int>(renderScale) },
	{ "game.rom_file", new BasicConfigurationOption<std::string>(romFileName) },
	{ "video.scanlines", new BasicConfigurationOption<bool>(scanlinesEnabled) },
	{ "video.vsync", new BasicConfigurationOption<bool>(vsyncEnabled) }
};

/**
 * Whether audio is enabled or not.
 */
BasicConfigurationOption<bool> Configuration::audioEnabled(
	"audio.enabled", true
);

/**
 * Audio frequency, in Hz
 */
BasicConfigurationOption<int> Configuration::audioFrequency(
	"audio.frequency", 48000
);

/**
 * Frame rate (per second).
 */
BasicConfigurationOption<int> Configuration::frameRate(
	"game.frame_rate", 60
);

/**
 * The filename for a custom palette to use for rendering.
 */
BasicConfigurationOption<std::string> Configuration::paletteFileName(
	"video.palette_file", ""
);

/**
 * Scaling factor for rendering.
 */
BasicConfigurationOption<int> Configuration::renderScale(
	"video.scale", 4
);

/**
 * Filename for the SMB ROM image.
 */
BasicConfigurationOption<std::string> Configuration::romFileName(
	"game.rom_file", "Super Mario Bros. (JU) (PRG0) [!].nes"
);

/**
 * Whether scanlines are enabled or not.
 */
BasicConfigurationOption<bool> Configuration::scanlinesEnabled(
	"video.scanlines", false
);

/**
 * Whether vsync is enabled for video.
 */
BasicConfigurationOption<bool> Configuration::vsyncEnabled(
	"video.vsync", true
);

ConfigurationOption::ConfigurationOption(
	const std::string& path) :
	path(path)
{
}

const std::string& ConfigurationOption::getPath() const
{
	return path;
}

void Configuration::initialize(const std::string& fileName)
{
	// Check that the configuration file exists.
	// If it does not exist, we will fall back to default values.
	std::ifstream configFile(fileName.c_str());
	if (configFile.good())
	{
		std::string line;
		std::string section;
		auto is_not_space = [](int ch) { return !std::isspace(ch); };

		while (std::getline(configFile, line))
		{
			// Remove leading and trailing whitespace
			line.erase(line.begin(), std::find_if(line.begin(), line.end(), is_not_space));
			line.erase(std::find_if(line.rbegin(), line.rend(), is_not_space).base(), line.end());

			// Ignore comments and empty lines
			if (!line.empty() && line[0] != ';')
			{
				// Check if this is a section
				if (line[0] == '[' && line[line.length() - 1] == ']')
				{
					section = line.substr(1, line.length() - 2);

					// Remove leading and trailing whitespace
					section.erase(section.begin(), std::find_if(section.begin(), section.end(), is_not_space));
					section.erase(std::find_if(section.rbegin(), section.rend(), is_not_space).base(), section.end());
				}
				else
				{
					// Split the line into key and value
					auto pos = line.find('=');
					if (pos != std::string::npos)
					{
						std::string key = line.substr(0, pos);
						std::string value = line.substr(pos + 1);

						// Remove trailing whitespace from the key
						key.erase(std::find_if(key.rbegin(), key.rend(), is_not_space).base(), key.end());

						// Initialize the value for the given section.key
						auto option = configurationOptions.find(section + "." + key);
						if (option != configurationOptions.end())
						{
							option->second->initializeValue(value);
						}
					}
				}
			}
		}
	}
}

bool Configuration::getAudioEnabled()
{
	return audioEnabled.getValue();
}

int Configuration::getAudioFrequency()
{
	return audioFrequency.getValue();
}

int Configuration::getFrameRate()
{
	return frameRate.getValue();
}

const std::string& Configuration::getPaletteFileName()
{
	return paletteFileName.getValue();
}

int Configuration::getRenderScale()
{
	return renderScale.getValue();
}

const std::string& Configuration::getRomFileName()
{
	return romFileName.getValue();
}

bool Configuration::getScanlinesEnabled()
{
	return scanlinesEnabled.getValue();
}

bool Configuration::getVsyncEnabled()
{
	return vsyncEnabled.getValue();
}
