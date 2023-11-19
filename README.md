# Haskell Consumptio/Production Simulator

Haskell MQTT Simulator is a project that simulates MQTT messages for consumption and production metrics. It uses Haskell for the simulation the consumption and production of electricity and MQTT for message communication.

## Table of Contents

- [Introduction](#introduction)
- [Features](#features)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
- [Usage](#usage)
- [Configuration](#configuration)
- [Contributing](#contributing)
- [License](#license)

## Introduction

The Haskell MQTT Simulator generates simulated MQTT messages representing consumption and production metrics. These messages are based on CSV data and are sent to an MQTT broker for further processing.

## Features

- Generation of simulated consumption and production metrics
- MQTT message publication
- CSV parsing and processing for consumption and production metrics

## Getting Started

### Prerequisites

Make sure you have the following software installed before running the simulator:

- Haskell
- MQTT broker (e.g., Mosquitto)

### Installation

Clone the repository and install the required Haskell packages:

```bash
git clone https://github.com/yourusername/haskell-mqtt-simulator.git
cd haskell-mqtt-simulator
stack build
```
## Usage

To run the simulator, provide a CSV file containing consumption data and configure the MQTT broker URI. For example:

```bash
stack run
```

## Configuration

You can configure the MQTT broker URI and other settings in the source code.

## Contributing

Contributions are welcome! If you find any issues or have suggestions for improvements, please open an issue or submit a pull request.

## License

This project is licensed under the BSD-3-Clause License - see the LICENSE file for details.