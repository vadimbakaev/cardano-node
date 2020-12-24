def aux_profiles:
[ { name: "short"
  , generator: { tx_count: 10000, inputs_per_tx: 1, outputs_per_tx: 1,  tps: 100 }
  }
, { name: "small"
  , generator: { tx_count: 1000,  inputs_per_tx: 1, outputs_per_tx: 1,  tps: 100
               , init_cooldown: 25 }
  , tolerances: { finish_patience: 4 }
  }
, { name: "smoke"
  , generator: { tx_count: 100,   add_tx_size: 0, inputs_per_tx: 1, outputs_per_tx: 1,  tps: 100
               , init_cooldown: 25 }
  , tolerances: { finish_patience: 4 }
  }
, { name: "default"
  , genesis:
    {
    }
  }
];
