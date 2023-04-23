#ifndef CONFIGURATION_HPP
#define CONFIGURATION_HPP

#include <iostream>
#include <map>
#include <string>

/**
 * Base class for configuration options.
 */
class ConfigurationOption
{
public:
	/**
	 * Construct a configuration option.
	 * 
	 * @param path the path to the option in the conf file,
	 * in the format "section.key" where section is the
	 * INI section (e.g. [example]) and key is the option
	 * name in the section.
	 */
	ConfigurationOption(const std::string& path);
	
	/**
	 * Get the path of the configuration option within the INI file.
	 */
	const std::string& getPath() const;

	/**
	 * Initialize the configuration option from the parsed property tree.
	 */
	virtual void initializeValue(const std::string& value)=0;

private:
	std::string path;
};

/**
 * Basic configuration option template for values that have simple types and only a default value.
 */
template <typename T>
class BasicConfigurationOption : public ConfigurationOption
{
public:
	/**
	 * Constructor.
	 */
	BasicConfigurationOption(const std::string& path, const T& defaultValue) :
		ConfigurationOption(path), value(defaultValue)
	{
	}
	
	/**
	 * Get the value of the configuration option.
	 */
	const T& getValue() const
	{
		return value;
	}

	/**
	 * Initialize the configuration option.
	 */
	void initializeValue(const std::string& value) override
	{
		if constexpr (std::is_same_v<T, bool>)
		{
			this->value = (value == "1" || value == "true");
		}
		else if constexpr (std::is_same_v<T, int>)
		{
			this->value = std::stoi(value);
		}
		else if constexpr (std::is_same_v<T, std::string>)
		{
			if (value.length() >= 2 && (value.front() == '"' && value.back() == '"'
				|| value.front() == '\'' && value.back() == '\''))
			{
				this->value = value.substr(1, value.length() - 2);
			}
			else
			{
				this->value = value;
			}
		}
		else
		{
			static_assert(!std::is_same_v<T, bool> && !std::is_same_v<T, int> && !std::is_same_v<T, std::string>,
				"Unsupported type for configuration option");
		}
	}

private:
	T value;
};

/**
 * Singleton class that reads the configuration file that provides global
 * program options from the user.
 */
class Configuration
{
public:
	/**
	 * Initialize the global configuration from the given file.
	 */
	static void initialize(const std::string& fileName);

	/**
	 * Get if audio is enabled or not.
	 */
	static bool getAudioEnabled();

	/**
	 * Get the desired audio frequency, in Hz.
	 */
	static int getAudioFrequency();

	/**
	 * Get the desired frame rate (per second).
	 */
	static int getFrameRate();

	/**
	 * Get the filename for a custom palette to use for rendering.
	 */
	static const std::string& getPaletteFileName();

	/**
	 * Get the desired ROM file name.
	 */
	static const std::string& getRomFileName();

	/**
	 * Get the desired render scale.
	 */
	static int getRenderScale();

	/**
	 * Get whether scanlines are enabled or not.
	 */
	static bool getScanlinesEnabled();

	/**
	 * Get whether vsync is enabled or not.
	 */
	static bool getVsyncEnabled();

private:
	static BasicConfigurationOption<bool> audioEnabled;
	static BasicConfigurationOption<int> audioFrequency;
	static BasicConfigurationOption<int> frameRate;
	static BasicConfigurationOption<std::string> paletteFileName;
	static BasicConfigurationOption<int> renderScale;
	static BasicConfigurationOption<std::string> romFileName;
	static BasicConfigurationOption<bool> scanlinesEnabled;
	static BasicConfigurationOption<bool> vsyncEnabled;

	static std::map<std::string, ConfigurationOption*> configurationOptions;
};

#endif // CONFIGURATION_HPP
