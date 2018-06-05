# Haskell client for OpenAI gym

## Usage

```
python3 ../gym_http_server.py
stylish-haskell
hlint . --report
stack haddock
stack build && stack exec example
stack build --file-watch
stack test --file-watch
stack exec example -- -v -a random --game CartPole-v0
```

## TODO

- placeholders
- actions/observations
- TensorFlow/HaskTorch/hmatrix
- agents
  - baselines
  - retro-baselines
  - sonic-on-ray
- viz
  - render
    - Tensorboard
    - heatmap
- algorithms
  - doc more
  - check impl
  - make impl
- tweaking
  - reward engineering
    - inverse RL (IRL) infers reward function from human demo
  - gym wrappers
    - observation
    - reward
    - action
    - time limit
    - dict
    - monitor
    - video recorder
    - stats recorder
    - retro sonic_util
  - optimizers
    - Ray RLlib
    - SLM-Lab
- env spec
- contest
- [exploration sampling](https://en.wikipedia.org/wiki/Active_learning_(machine_learning)#Query_strategies)
- NN optimizers
- MDP extensions as listed at /media/tycho/Drogon/Coding/ml/reinforcement-learning/manning-gdrl/manning%20RL.pdf page 47
  - horizon (infinite ~ greedy)in episodic (i/o continuing) tasks
  - stochastic environment
  - stochastic action selection (property of agent, not MDP)
  - state
  - Partially-Observable Markov Decision Process (POMDP): When the agent cannot fully observe the environment state.
  - Factored Markov Decision Process (FMDP): Allows the representation of the transition and reward function more compactly so that we can represent very large MDPs.
  - Continuous [Time|Action|State] Markov Decision Process: When either time, action, state or any combination of them are continuous.
  - Relational Markov Decision Process (RMDP): Allows the combination of probabilistic and relational knowledge.
  - Semi-Markov Decision Process (SMDP): Allows the inclusion of abstract actions that can take multiple timesteps to complete.
  - Multi-Agent Markov Decision Process (MMDP): Allows the inclusion of multiple agents in the same environment.
  - Decentralized Markov Decision Process (Dec-MDP): Allows for multiple agents to collaborate and maximize a common reward.
- transfer learning
- human priors
- fpga (Clash compiler)
