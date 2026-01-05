// Azimuth 游戏理论测试用例
// 专注于游戏理论算法和概念

// 测试1: 纳什均衡计算
test "纳什均衡计算" {
  // 收益矩阵表示
  type PayoffMatrix = {
    player1: Array<Array<Float>>,  // 玩家1的收益矩阵
    player2: Array<Array<Float>>   // 玩家2的收益矩阵
  }
  
  // 策略组合
  type StrategyProfile = {
    player1_strategy: Int,  // 玩家1的策略索引
    player2_strategy: Int   // 玩家2的策略索引索引
  }
  
  // 混合策略
  type MixedStrategy = {
    player1_probabilities: Array<Float>,  // 玩家1各策略的概率
    player2_probabilities: Array<Float>   // 玩家2各策略的概率
  }
  
  // 检查纯策略纳什均衡
  let find_pure_strategy_nash_equilibrium = fn(payoff_matrix: PayoffMatrix) {
    let num_strategies_p1 = payoff_matrix.player1.length()
    let num_strategies_p2 = payoff_matrix.player1[0].length()
    let mut equilibria = []
    
    for i in 0..num_strategies_p1 {
      for j in 0..num_strategies_p2 {
        let mut is_best_response_p1 = true
        let mut is_best_response_p2 = true
        
        // 检查玩家1的最佳响应
        for k in 0..num_strategies_p1 {
          if payoff_matrix.player1[k][j] > payoff_matrix.player1[i][j] {
            is_best_response_p1 = false
            break
          }
        }
        
        // 检查玩家2的最佳响应
        for k in 0..num_strategies_p2 {
          if payoff_matrix.player2[i][k] > payoff_matrix.player2[i][j] {
            is_best_response_p2 = false
            break
          }
        }
        
        if is_best_response_p1 && is_best_response_p2 {
          equilibria = equilibria.push({ player1_strategy: i, player2_strategy: j })
        }
      }
    }
    
    equilibria
  }
  
  // 计算期望收益
  let calculate_expected_payoff = fn(payoff_matrix: PayoffMatrix, mixed_strategy: MixedStrategy, player: Int) {
    let mut expected_payoff = 0.0
    
    for i in 0..mixed_strategy.player1_probabilities.length() {
      for j in 0..mixed_strategy.player2_probabilities.length() {
        let probability = mixed_strategy.player1_probabilities[i] * mixed_strategy.player2_probabilities[j]
        let payoff = if player == 1 { 
          payoff_matrix.player1[i][j] 
        } else { 
          payoff_matrix.player2[i][j] 
        }
        expected_payoff = expected_payoff + probability * payoff
      }
    }
    
    expected_payoff
  }
  
  // 简化的混合策略纳什均衡计算（仅适用于2x2博弈）
  let find_mixed_strategy_nash_equilibrium = fn(payoff_matrix: PayoffMatrix) {
    // 仅适用于2x2博弈
    if payoff_matrix.player1.length() != 2 || payoff_matrix.player1[0].length() != 2 {
      return None
    }
    
    // 计算玩家1的混合策略概率
    let a = payoff_matrix.player2[0][0]
    let b = payoff_matrix.player2[0][1]
    let c = payoff_matrix.player2[1][0]
    let d = payoff_matrix.player2[1][1]
    
    let p1_prob = if (d - b) - (c - a) != 0.0 {
      (d - b) / ((d - b) - (c - a))
    } else {
      0.5  // 默认值
    }
    
    // 计算玩家2的混合策略概率
    let e = payoff_matrix.player1[0][0]
    let f = payoff_matrix.player1[1][0]
    let g = payoff_matrix.player1[0][1]
    let h = payoff_matrix.player1[1][1]
    
    let p2_prob = if (h - f) - (g - e) != 0.0 {
      (h - f) / ((h - f) - (g - e))
    } else {
      0.5  // 默认值
    }
    
    Some({
      player1_probabilities: [p1_prob, 1.0 - p1_prob],
      player2_probabilities: [p2_prob, 1.0 - p2_prob]
    })
  }
  
  // 测试囚徒困境
  let prisoners_dilemma = {
    player1: [
      [3.0, 0.0],  // 合作/合作, 合作/背叛
      [5.0, 1.0]   // 背叛/合作, 背叛/背叛
    ],
    player2: [
      [3.0, 5.0],  // 合作/合作, 背叛/合作
      [0.0, 1.0]   // 合作/背叛, 背叛/背叛
    ]
  }
  
  let pd_equilibria = find_pure_strategy_nash_equilibrium(prisoners_dilemma)
  
  assert_eq(pd_equilibria.length(), 1)
  assert_eq(pd_equilibria[0].player1_strategy, 1)  // 背叛
  assert_eq(pd_equilibria[0].player2_strategy, 1)  // 背叛
  
  // 测试性别战博弈
  let battle_of_sexes = {
    player1: [
      [2.0, 0.0],  // 足球/足球, 足球/歌剧
      [0.0, 1.0]   // 歌剧/足球, 歌剧/歌剧
    ],
    player2: [
      [1.0, 0.0],  // 足球/足球, 足球/歌剧
      [0.0, 2.0]   // 歌剧/足球, 歌剧/歌剧
    ]
  }
  
  let bos_equilibria = find_pure_strategy_nash_equilibrium(battle_of_sexes)
  
  assert_eq(bos_equilibria.length(), 2)
  // 两个纯策略纳什均衡：(足球,足球)和(歌剧,歌剧)
  
  // 测试混合策略纳什均衡
  let bos_mixed = find_mixed_strategy_nash_equilibrium(battle_of_sexes)
  
  assert_true(bos_mixed.is_some())
  match bos_mixed {
    Some(mixed) => {
      // 对于性别战博弈，玩家1选择足球的概率应该是2/3
      assert_true(mixed.player1_probabilities[0] > 0.6 && mixed.player1_probabilities[0] < 0.7)
      // 玩家2选择足球的概率应该是1/3
      assert_true(mixed.player2_probabilities[0] > 0.3 && mixed.player2_probabilities[0] < 0.4)
    }
    None => assert_true(false)
  }
  
  // 测试期望收益计算
  match bos_mixed {
    Some(mixed) => {
      let expected_p1 = calculate_expected_payoff(battle_of_sexes, mixed, 1)
      let expected_p2 = calculate_expected_payoff(battle_of_sexes, mixed, 2)
      
      // 在混合策略纳什均衡中，两个玩家的期望收益应该相等
      assert_true((expected_p1 - expected_p2).abs() < 0.1)
    }
    None => assert_true(false)
  }
}

// 测试2: 最小最大算法和博弈树搜索
test "最小最大算法和博弈树搜索" {
  // 博弈树节点
  type GameNode = {
    value: Option<Float>,  // 终节点的值
    children: Array<GameNode>,
    is_max_player: Bool    // 是否是最大玩家的回合
  }
  
  // 创建简单博弈树
  let create_simple_game_tree = fn() {
    {
      value: None,
      children: [
        {
          value: None,
          children: [
            { value: Some(3.0), children: [], is_max_player: false },
            { value: Some(5.0), children: [], is_max_player: false }
          ],
          is_max_player: true
        },
        {
          value: None,
          children: [
            { value: Some(6.0), children: [], is_max_player: false },
            { value: Some(9.0), children: [], is_max_player: false }
          ],
          is_max_player: true
        },
        {
          value: None,
          children: [
            { value: Some(1.0), children: [], is_max_player: false },
            { value: Some(2.0), children: [], is_max_player: false }
          ],
          is_max_player: true
        }
      ],
      is_max_player: false
    }
  }
  
  // 最小最大算法
  let minimax = fn(node: GameNode, alpha: Float, beta: Float, is_maximizing: Bool) {
    match node.value {
      Some(value) => value
      None => {
        if is_maximizing {
          let mut max_eval = -Float::max_value()
          
          for child in node.children {
            let eval = minimax(child, alpha, beta, false)
            max_eval = max_eval.max(eval)
            alpha = alpha.max(eval)
            
            if beta <= alpha {
              break  // Alpha-beta剪枝
            }
          }
          
          max_eval
        } else {
          let mut min_eval = Float::max_value()
          
          for child in node.children {
            let eval = minimax(child, alpha, beta, true)
            min_eval = min_eval.min(eval)
            beta = beta.min(eval)
            
            if beta <= alpha {
              break  // Alpha-beta剪枝
            }
          }
          
          min_eval
        }
      }
    }
  }
  
  // 找到最佳移动
  let find_best_move = fn(node: GameNode) {
    let mut best_value = if node.is_max_player { -Float::max_value() } else { Float::max_value() }
    let mut best_move_index = -1
    
    for i in 0..node.children.length() {
      let child = node.children[i]
      let value = minimax(child, -Float::max_value(), Float::max_value(), not(node.is_max_player))
      
      if node.is_max_player && value > best_value {
        best_value = value
        best_move_index = i
      } else if not(node.is_max_player) && value < best_value {
        best_value = value
        best_move_index = i
      }
    }
    
    (best_move_index, best_value)
  }
  
  // 测试最小最大算法
  let game_tree = create_simple_game_tree()
  
  let root_value = minimax(game_tree, -Float::max_value(), Float::max_value(), true)
  
  // 根节点应该是最大玩家，所以会选择路径中的最大值
  // 路径1: min(3, 5) = 3
  // 路径2: min(6, 9) = 6
  // 路径3: min(1, 2) = 1
  // max(3, 6, 1) = 6
  assert_eq(root_value, 6.0)
  
  // 测试最佳移动
  let (best_move_index, best_value) = find_best_move(game_tree)
  
  assert_eq(best_move_index, 1)  // 第二个子节点（包含6和9）
  assert_eq(best_value, 6.0)
  
  // 测试更复杂的博弈树
  let complex_game_tree = {
    value: None,
    children: [
      {
        value: None,
        children: [
          {
            value: None,
            children: [
              { value: Some(5.0), children: [], is_max_player: true },
              { value: Some(6.0), children: [], is_max_player: true }
            ],
            is_max_player: false
          },
          {
            value: None,
            children: [
              { value: Some(7.0), children: [], is_max_player: true },
              { value: Some(4.0), children: [], is_max_player: true }
            ],
            is_max_player: false
          }
        ],
        is_max_player: true
      },
      {
        value: None,
        children: [
          {
            value: None,
            children: [
              { value: Some(3.0), children: [], is_max_player: true },
              { value: Some(9.0), children: [], is_max_player: true }
            ],
            is_max_player: false
          }
        ],
        is_max_player: true
      }
    ],
    is_max_player: false
  }
  
  let complex_root_value = minimax(complex_game_tree, -Float::max_value(), Float::max_value(), true)
  
  // 计算复杂博弈树的最小最大值
  // 左子树：max(min(5,6), min(7,4)) = max(5, 4) = 5
  // 右子树：min(3, 9) = 3
  // max(5, 3) = 5
  assert_eq(complex_root_value, 5.0)
}

// 测试3: 拍卖理论和机制设计
test "拍卖理论和机制设计" {
  // 竞拍者
  type Bidder = {
    id: String,
    valuation: Float,  // 对物品的估值
    bid: Float         // 出价
  }
  
  // 拍卖结果
  type AuctionResult = {
    winner: String,
    winning_bid: Float,
    second_highest_bid: Float,
    revenue: Float
  }
  
  // 英式拍卖（价格递增）
  let english_auction = fn(bidders: Array<Bidder>, reserve_price: Float) {
    let mut current_price = reserve_price
    let mut active_bidders = bidders
    let mut winner = ""
    
    while active_bidders.length() > 1 {
      let mut remaining_bidders = []
      
      for bidder in active_bidders {
        if bidder.valuation >= current_price {
          remaining_bidders = remaining_bidders.push(bidder)
        }
      }
      
      if remaining_bidders.length() <= 1 {
        winner = if remaining_bidders.length() == 1 { 
          remaining_bidders[0].id 
        } else { 
          "" 
        }
        break
      }
      
      active_bidders = remaining_bidders
      current_price = current_price + 1.0  // 价格递增
    }
    
    if winner != "" {
      {
        winner: winner,
        winning_bid: current_price,
        second_highest_bid: current_price - 1.0,
        revenue: current_price
      }
    } else {
      {
        winner: "",
        winning_bid: 0.0,
        second_highest_bid: 0.0,
        revenue: 0.0
      }
    }
  }
  
  // 荷式拍卖（价格递减）
  let dutch_auction = fn(bidders: Array<Bidder>, start_price: Float, decrement: Float) {
    let mut current_price = start_price
    let mut winner = ""
    
    // 按估值降序排列竞拍者
    let mut sorted_bidders = bidders
    let mut i = 0
    while i < sorted_bidders.length() - 1 {
      let mut j = i + 1
      while j < sorted_bidders.length() {
        if sorted_bidders[i].valuation < sorted_bidders[j].valuation {
          let temp = sorted_bidders[i]
          sorted_bidders[i] = sorted_bidders[j]
          sorted_bidders[j] = temp
        }
        j = j + 1
      }
      i = i + 1
    }
    
    for bidder in sorted_bidders {
      if bidder.valuation >= current_price {
        winner = bidder.id
        break
      }
      current_price = current_price - decrement
    }
    
    if winner != "" {
      {
        winner: winner,
        winning_bid: current_price,
        second_highest_bid: current_price,
        revenue: current_price
      }
    } else {
      {
        winner: "",
        winning_bid: 0.0,
        second_highest_bid: 0.0,
        revenue: 0.0
      }
    }
  }
  
  // 维克里拍卖（第二价格密封投标）
  let vickrey_auction = fn(bidders: Array<Bidder>) {
    if bidders.length() == 0 {
      return {
        winner: "",
        winning_bid: 0.0,
        second_highest_bid: 0.0,
        revenue: 0.0
      }
    }
    
    // 按出价降序排列
    let mut sorted_bidders = bidders
    let mut i = 0
    while i < sorted_bidders.length() - 1 {
      let mut j = i + 1
      while j < sorted_bidders.length() {
        if sorted_bidders[i].bid < sorted_bidders[j].bid {
          let temp = sorted_bidders[i]
          sorted_bidders[i] = sorted_bidders[j]
          sorted_bidders[j] = temp
        }
        j = j + 1
      }
      i = i + 1
    }
    
    let winner = sorted_bidders[0].id
    let winning_bid = sorted_bidders[0].bid
    let second_highest_bid = if sorted_bidders.length() > 1 { 
      sorted_bidders[1].bid 
    } else { 
      0.0 
    }
    
    {
      winner: winner,
      winning_bid: winning_bid,
      second_highest_bid: second_highest_bid,
      revenue: second_highest_bid
    }
  }
  
  // 计算社会福利
  let calculate_social_welfare = fn(bidders: Array<Bidder>, result: AuctionResult) {
    if result.winner == "" {
      return 0.0
    }
    
    let mut winner_valuation = 0.0
    for bidder in bidders {
      if bidder.id == result.winner {
        winner_valuation = bidder.valuation
        break
      }
    }
    
    winner_valuation - result.winning_bid
  }
  
  // 测试英式拍卖
  let english_bidders = [
    { id: "Alice", valuation: 100.0, bid: 0.0 },
    { id: "Bob", valuation: 80.0, bid: 0.0 },
    { id: "Charlie", valuation: 120.0, bid: 0.0 }
  ]
  
  let english_result = english_auction(english_bidders, 50.0)
  
  assert_eq(english_result.winner, "Charlie")  // 估值最高者获胜
  assert_eq(english_result.winning_bid, 101.0)  // 超过次高估值的最低价格
  assert_eq(english_result.revenue, 101.0)
  
  // 测试荷式拍卖
  let dutch_bidders = [
    { id: "Alice", valuation: 100.0, bid: 0.0 },
    { id: "Bob", valuation: 80.0, bid: 0.0 },
    { id: "Charlie", valuation: 120.0, bid: 0.0 }
  ]
  
  let dutch_result = dutch_auction(dutch_bidders, 150.0, 5.0)
  
  assert_eq(dutch_result.winner, "Charlie")  // 估值最高者获胜
  assert_eq(dutch_result.winning_bid, 120.0)  // 等于最高估值的价格
  assert_eq(dutch_result.revenue, 120.0)
  
  // 测试维克里拍卖
  let vickrey_bidders = [
    { id: "Alice", valuation: 100.0, bid: 95.0 },
    { id: "Bob", valuation: 80.0, bid: 85.0 },
    { id: "Charlie", valuation: 120.0, bid: 110.0 }
  ]
  
  let vickrey_result = vickrey_auction(vickrey_bidders)
  
  assert_eq(vickrey_result.winner, "Charlie")  // 出价最高者获胜
  assert_eq(vickrey_result.winning_bid, 110.0)  // 最高出价
  assert_eq(vickrey_result.second_highest_bid, 95.0)  // 次高出价
  assert_eq(vickrey_result.revenue, 95.0)  // 支付次高出价
  
  // 测试社会福利计算
  let english_social_welfare = calculate_social_welfare(english_bidders, english_result)
  let dutch_social_welfare = calculate_social_welfare(dutch_bidders, dutch_result)
  let vickrey_social_welfare = calculate_social_welfare(vickrey_bidders, vickrey_result)
  
  // 在维克里拍卖中，竞拍者有动机如实出价，所以社会福利应该最高
  assert_true(vickrey_social_welfare >= english_social_welfare)
  assert_true(vickrey_social_welfare >= dutch_social_welfare)
  
  // 测试无保留价情况
  let no_reserve_bidders = [
    { id: "Alice", valuation: 10.0, bid: 0.0 },
    { id: "Bob", valuation: 5.0, bid: 0.0 }
  ]
  
  let no_reserve_result = english_auction(no_reserve_bidders, 0.0)
  
  assert_eq(no_reserve_result.winner, "Alice")
  assert_eq(no_reserve_result.winning_bid, 6.0)  // 超过Bob估值的最低价格
}

// 测试4: 合作博弈和夏普利值
test "合作博弈和夏普利值" {
  // 合作博弈
  type CooperativeGame = {
    players: Array<String>,
    coalition_values: { Array<String>: Float }  // 联盟的价值
  }
  
  // 计算夏普利值
  let calculate_shapley_value = fn(game: CooperativeGame, player: String) {
    let mut shapley_value = 0.0
    let n = game.players.length()
    
    // 生成所有可能的联盟
    let all_coalitions = generate_all_coalitions(game.players)
    
    for coalition in all_coalitions {
      if not(coalition.contains(player)) {
        // 计算添加玩家前后的边际贡献
        let value_without_player = get_coalition_value(game, coalition)
        
        let coalition_with_player = coalition.push(player)
        let value_with_player = get_coalition_value(game, coalition_with_player)
        
        let marginal_contribution = value_with_player - value_without_player
        
        // 计算权重
        let coalition_size = coalition.length()
        let weight = (coalition_size.to_float().factorial() * 
                     ((n - coalition_size - 1).to_float().factorial())) / 
                    n.to_float().factorial()
        
        shapley_value = shapley_value + weight * marginal_contribution
      }
    }
    
    shapley_value
  }
  
  // 生成所有可能的联盟
  let generate_all_coalitions = fn(players: Array<String>) {
    let mut all_coalitions = [[]]
    
    for player in players {
      let mut new_coalitions = []
      
      for coalition in all_coalitions {
        new_coalitions = new_coalitions.push(coalition.push(player))
      }
      
      all_coalitions = all_coalitions + new_coalitions
    }
    
    all_coalitions
  }
  
  // 获取联盟价值
  let get_coalition_value = fn(game: CooperativeGame, coalition: Array<String>) {
    let mut sorted_coalition = coalition
    let mut i = 0
    while i < sorted_coalition.length() - 1 {
      let mut j = i + 1
      while j < sorted_coalition.length() {
        if sorted_coalition[i] > sorted_coalition[j] {
          let temp = sorted_coalition[i]
          sorted_coalition[i] = sorted_coalition[j]
          sorted_coalition[j] = temp
        }
        j = j + 1
      }
      i = i + 1
    }
    
    match game.coalition_values[sorted_coalition] {
      Some(value) => value
      None => 0.0
    }
  }
  
  // 计算所有玩家的夏普利值
  let calculate_all_shapley_values = fn(game: CooperativeGame) {
    let mut shapley_values = {}
    
    for player in game.players {
      shapley_values[player] = calculate_shapley_value(game, player)
    }
    
    shapley_values
  }
  
  // 测试简单三人合作博弈
  let three_player_game = {
    players: ["A", "B", "C"],
    coalition_values: {
      ["A"]: 10.0,
      ["B"]: 20.0,
      ["C"]: 30.0,
      ["A", "B"]: 40.0,
      ["A", "C"]: 50.0,
      ["B", "C"]: 70.0,
      ["A", "B", "C"]: 100.0
    }
  }
  
  let shapley_values = calculate_all_shapley_values(three_player_game)
  
  // 验证夏普利值的总和等于大联盟的价值
  let total_shapley = shapley_values["A"] + shapley_values["B"] + shapley_values["C"]
  assert_eq(total_shapley, 100.0)
  
  // 验证夏普利值的效率性
  assert_true((total_shapley - 100.0).abs() < 0.001)
  
  // 测试对称性：如果两个玩家在所有联盟中的贡献相同，他们的夏普利值应该相同
  let symmetric_game = {
    players: ["X", "Y", "Z"],
    coalition_values: {
      ["X"]: 10.0,
      ["Y"]: 10.0,
      ["Z"]: 5.0,
      ["X", "Y"]: 25.0,
      ["X", "Z"]: 20.0,
      ["Y", "Z"]: 20.0,
      ["X", "Y", "Z"]: 40.0
    }
  }
  
  let symmetric_shapley = calculate_all_shapley_values(symmetric_game)
  
  // X和Y是对称的，所以他们的夏普利值应该相同
  assert_eq(symmetric_shapley["X"], symmetric_shapley["Y"])
  
  // 测试虚拟玩家：如果一个玩家对任何联盟都没有贡献，其夏普利值应该为0
  let dummy_player_game = {
    players: ["P", "Q", "D"],  // D是虚拟玩家
    coalition_values: {
      ["P"]: 15.0,
      ["Q"]: 25.0,
      ["D"]: 0.0,
      ["P", "Q"]: 45.0,
      ["P", "D"]: 15.0,
      ["Q", "D"]: 25.0,
      ["P", "Q", "D"]: 45.0
    }
  }
  
  let dummy_shapley = calculate_all_shapley_values(dummy_player_game)
  
  // D是虚拟玩家，所以其夏普利值应该为0
  assert_eq(dummy_shapley["D"], 0.0)
  
  // 测试可加性：两个博弈的和的夏普利值等于各自夏普利值的和
  let game1 = {
    players: ["A", "B"],
    coalition_values: {
      ["A"]: 5.0,
      ["B"]: 10.0,
      ["A", "B"]: 20.0
    }
  }
  
  let game2 = {
    players: ["A", "B"],
    coalition_values: {
      ["A"]: 3.0,
      ["B"]: 7.0,
      ["A", "B"]: 15.0
    }
  }
  
  let shapley1 = calculate_all_shapley_values(game1)
  let shapley2 = calculate_all_shapley_values(game2)
  
  // 创建和博弈
  let sum_game = {
    players: ["A", "B"],
    coalition_values: {
      ["A"]: 8.0,   // 5.0 + 3.0
      ["B"]: 17.0,  // 10.0 + 7.0
      ["A", "B"]: 35.0  // 20.0 + 15.0
    }
  }
  
  let shapley_sum = calculate_all_shapley_values(sum_game)
  
  // 验证可加性
  assert_eq(shapley_sum["A"], shapley1["A"] + shapley2["A"])
  assert_eq(shapley_sum["B"], shapley1["B"] + shapley2["B"])
}

// 测试5: 演化博弈理论
test "演化博弈理论" {
  // 策略
  type Strategy = String
  
  // 收益矩阵
  type PayoffMatrix = {
    strategies: Array<Strategy>,
    payoffs: { (Strategy, Strategy): Float }  // (行策略, 列策略) -> 收益
  }
  
  // 种群状态
  type PopulationState = {
    strategy_frequencies: { Strategy: Float },  // 各策略的频率
    total_population: Int
  }
  
  // 计算策略的期望收益
  let calculate_expected_payoff = fn(payoff_matrix: PayoffMatrix, population_state: PopulationState, strategy: Strategy) {
    let mut expected_payoff = 0.0
    
    for (other_strategy, frequency) in population_state.strategy_frequencies {
      match payoff_matrix.payoffs[(strategy, other_strategy)] {
        Some(payoff) => {
          expected_payoff = expected_payoff + payoff * frequency
        }
        None => ()
      }
    }
    
    expected_payoff
  }
  
  // 计算种群平均收益
  let calculate_average_payoff = fn(payoff_matrix: PayoffMatrix, population_state: PopulationState) {
    let mut average_payoff = 0.0
    
    for (strategy, frequency) in population_state.strategy_frequencies {
      let strategy_payoff = calculate_expected_payoff(payoff_matrix, population_state, strategy)
      average_payoff = average_payoff + strategy_payoff * frequency
    }
    
    average_payoff
  }
  
  // 复制者动态
  let replicator_dynamics = fn(payoff_matrix: PayoffMatrix, population_state: PopulationState, time_step: Float) {
    let mut new_frequencies = {}
    let average_payoff = calculate_average_payoff(payoff_matrix, population_state)
    
    for (strategy, frequency) in population_state.strategy_frequencies {
      let strategy_payoff = calculate_expected_payoff(payoff_matrix, population_state, strategy)
      let growth_rate = (strategy_payoff - average_payoff) * time_step
      let new_frequency = frequency * (1.0 + growth_rate)
      new_frequencies[strategy] = new_frequency
    }
    
    // 归一化频率
    let mut total_frequency = 0.0
    for (_, frequency) in new_frequencies {
      total_frequency = total_frequency + frequency
    }
    
    let mut normalized_frequencies = {}
    for (strategy, frequency) in new_frequencies {
      normalized_frequencies[strategy] = frequency / total_frequency
    }
    
    {
      strategy_frequencies: normalized_frequencies,
      total_population: population_state.total_population
    }
  }
  
  // 检查演化稳定策略
  let is_evolutionarily_stable = fn(payoff_matrix: PayoffMatrix, strategy: Strategy, mutant_strategy: Strategy) {
    let population_state = {
      strategy_frequencies: { strategy: 0.9, mutant_strategy: 0.1 },
      total_population: 100
    }
    
    let resident_payoff = calculate_expected_payoff(payoff_matrix, population_state, strategy)
    let mutant_payoff = calculate_expected_payoff(payoff_matrix, population_state, mutant_strategy)
    
    resident_payoff > mutant_payoff
  }
  
  // 测试鹰鸽博弈
  let hawk_dove_game = {
    strategies: ["Hawk", "Dove"],
    payoffs: {
      ("Hawk", "Hawk"): (V - C) / 2.0,  // V是资源价值，C是冲突成本
      ("Hawk", "Dove"): V,              // 鹰获胜，得到全部资源
      ("Dove", "Hawk"): 0.0,            // 鸽子退让，得到0
      ("Dove", "Dove"): V / 2.0         // 和平分享资源
    }
  }
  
  let V = 10.0  // 资源价值
  let C = 20.0  // 冲突成本
  
  let hawk_dove_payoffs = {
    strategies: ["Hawk", "Dove"],
    payoffs: {
      ("Hawk", "Hawk"): (V - C) / 2.0,  // (10-20)/2 = -5
      ("Hawk", "Dove"): V,              // 10
      ("Dove", "Hawk"): 0.0,            // 0
      ("Dove", "Dove"): V / 2.0         // 5
    }
  }
  
  // 初始种群状态：一半鹰，一半鸽
  let initial_population = {
    strategy_frequencies: { "Hawk": 0.5, "Dove": 0.5 },
    total_population: 100
  }
  
  // 计算期望收益
  let hawk_payoff = calculate_expected_payoff(hawk_dove_payoffs, initial_population, "Hawk")
  let dove_payoff = calculate_expected_payoff(hawk_dove_payoffs, initial_population, "Dove")
  
  // 鹰的期望收益：0.5 * (-5) + 0.5 * 10 = 2.5
  assert_eq(hawk_payoff, 2.5)
  
  // 鸽的期望收益：0.5 * 0 + 0.5 * 5 = 2.5
  assert_eq(dove_payoff, 2.5)
  
  // 计算平均收益
  let average_payoff = calculate_average_payoff(hawk_dove_payoffs, initial_population)
  assert_eq(average_payoff, 2.5)
  
  // 测试复制者动态
  let new_population = replicator_dynamics(hawk_dove_payoffs, initial_population, 0.1)
  
  // 由于初始状态下鹰和鸽的收益相同，频率应该不变
  assert_eq(new_population.strategy_frequencies["Hawk"], 0.5)
  assert_eq(new_population.strategy_frequencies["Dove"], 0.5)
  
  // 测试演化稳定策略
  // 在鹰鸽博弈中，没有纯策略ESS，但存在混合策略ESS
  // 鹰的频率应该是V/C = 10/20 = 0.5
  
  // 测试囚徒困境的演化稳定性
  let prisoner_dilemma_payoffs = {
    strategies: ["Cooperate", "Defect"],
    payoffs: {
      ("Cooperate", "Cooperate"): 3.0,
      ("Cooperate", "Defect"): 0.0,
      ("Defect", "Cooperate"): 5.0,
      ("Defect", "Defect"): 1.0
    }
  }
  
  // 在囚徒困境中，背叛是演化稳定策略
  let is_defect_ess = is_evolutionarily_stable(prisoner_dilemma_payoffs, "Defect", "Cooperate")
  let is_cooperate_ess = is_evolutionarily_stable(prisoner_dilemma_payoffs, "Cooperate", "Defect")
  
  assert_true(is_defect_ess)
  assert_false(is_cooperate_ess)
  
  // 测试协调博弈
  let coordination_payoffs = {
    strategies: ["StrategyA", "StrategyB"],
    payoffs: {
      ("StrategyA", "StrategyA"): 2.0,
      ("StrategyA", "StrategyB"): 0.0,
      ("StrategyB", "StrategyA"): 0.0,
      ("StrategyB", "StrategyB"): 1.0
    }
  }
  
  // 在协调博弈中，两个策略都是ESS，但StrategyA风险占优
  let is_a_ess = is_evolutionarily_stable(coordination_payoffs, "StrategyA", "StrategyB")
  let is_b_ess = is_evolutionarily_stable(coordination_payoffs, "StrategyB", "StrategyA")
  
  assert_true(is_a_ess)
  assert_true(is_b_ess)
  
  // 测试多轮演化
  let mut evolved_population = initial_population
  
  for i in 0..10 {
    evolved_population = replicator_dynamics(hawk_dove_payoffs, evolved_population, 0.1)
  }
  
  // 在鹰鸽博弈中，种群应该趋向于混合策略均衡
  assert_true(evolved_population.strategy_frequencies["Hawk"] > 0.4 && evolved_population.strategy_frequencies["Hawk"] < 0.6)
  assert_true(evolved_population.strategy_frequencies["Dove"] > 0.4 && evolved_population.strategy_frequencies["Dove"] < 0.6)
}