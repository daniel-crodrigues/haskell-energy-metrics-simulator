# Haskell Consumptio/Production Simulator

Haskell MQTT Simulator is a project that simulates MQTT messages for consumption and production metrics. It uses Haskell to simulate the consumption and production of electricity and MQTT for message communication.

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

The Haskell MQTT Simulator generates events about the energy consumption and production and sends that events throught MQTT messages.

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

## License

This project is licensed under the MIT License - see the LICENSE file for details.
