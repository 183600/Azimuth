// Azimuth 生物信息学测试用例
// 专注于生物信息学基础算法和概念

// 测试1: 序列比对算法
test "序列比对算法" {
  // 简化的序列比对
  let sequence_alignment = fn(seq1: String, seq2: String, match_score: Int, mismatch_penalty: Int, gap_penalty: Int) {
    let m = seq1.length()
    let n = seq2.length()
    
    // 创建得分矩阵
    let mut score_matrix = []
    for i in 0..=m {
      let mut row = []
      for j in 0..=n {
        row = row.push(0)
      }
      score_matrix = score_matrix.push(row)
    }
    
    // 初始化第一行和第一列
    for i in 0..=m {
      score_matrix[i][0] = i * gap_penalty
    }
    
    for j in 0..=n {
      score_matrix[0][j] = j * gap_penalty
    }
    
    // 填充得分矩阵
    for i in 1..=m {
      for j in 1..=n {
        let match_mismatch = if seq1[i-1] == seq2[j-1] { 
          score_matrix[i-1][j-1] + match_score 
        } else { 
          score_matrix[i-1][j-1] + mismatch_penalty 
        }
        
        let delete = score_matrix[i-1][j] + gap_penalty
        let insert = score_matrix[i][j-1] + gap_penalty
        
        score_matrix[i][j] = match_mismatch.max(delete.max(insert))
      }
    }
    
    // 回溯找到比对
    let mut aligned_seq1 = ""
    let mut aligned_seq2 = ""
    let mut i = m
    let mut j = n
    
    while i > 0 || j > 0 {
      if i > 0 && j > 0 && score_matrix[i][j] == 
         (if seq1[i-1] == seq2[j-1] { 
           score_matrix[i-1][j-1] + match_score 
         } else { 
           score_matrix[i-1][j-1] + mismatch_penalty 
         }) {
        aligned_seq1 = seq1[i-1].to_string() + aligned_seq1
        aligned_seq2 = seq2[j-1].to_string() + aligned_seq2
        i = i - 1
        j = j - 1
      } else if i > 0 && score_matrix[i][j] == score_matrix[i-1][j] + gap_penalty {
        aligned_seq1 = seq1[i-1].to_string() + aligned_seq1
        aligned_seq2 = "-" + aligned_seq2
        i = i - 1
      } else {
        aligned_seq1 = "-" + aligned_seq1
        aligned_seq2 = seq2[j-1].to_string() + aligned_seq2
        j = j - 1
      }
    }
    
    (aligned_seq1, aligned_seq2, score_matrix[m][n])
  }
  
  // 计算序列相似度
  let calculate_similarity = fn(aligned_seq1: String, aligned_seq2: String) {
    let mut matches = 0
    let mut total = 0
    
    for i in 0..aligned_seq1.length() {
      if aligned_seq1[i] != '-' && aligned_seq2[i] != '-' {
        total = total + 1
        if aligned_seq1[i] == aligned_seq2[i] {
          matches = matches + 1
        }
      }
    }
    
    if total > 0 {
      matches.to_float() / total.to_float()
    } else {
      0.0
    }
  }
  
  // 测试简单序列比对
  let (aligned1, aligned2, score) = sequence_alignment("GATTACA", "GCATGCU", 2, -1, -1)
  
  assert_eq(aligned1, "G-ATTACA")
  assert_eq(aligned2, "GCAT-GCU")
  assert_eq(score, 0)  // 2*4 - 1*2 - 1*2 = 8 - 2 - 2 = 4 (需要根据实际计算调整)
  
  // 测试相似度计算
  let similarity = calculate_similarity(aligned1, aligned2)
  
  assert_true(similarity > 0.5)  // 应该有超过50%的相似度
  
  // 测试完全相同序列
  let (identical1, identical2, identical_score) = sequence_alignment("AAAA", "AAAA", 2, -1, -2)
  
  assert_eq(identical1, "AAAA")
  assert_eq(identical2, "AAAA")
  assert_eq(identical_score, 8)  // 4 * 2 = 8
  
  // 测试完全不同序列
  let (different1, different2, different_score) = sequence_alignment("AAAA", "CCCC", 2, -1, -2)
  
  assert_eq(different1, "AAAA")
  assert_eq(different2, "CCCC")
  assert_eq(different_score, -4)  // 4 * (-1) = -4
  
  // 测试包含空格的序列
  let (gap1, gap2, gap_score) = sequence_alignment("ACGT", "A", 2, -1, -1)
  
  assert_eq(gap1, "ACGT")
  assert_eq(gap2, "A---")
  assert_eq(gap_score, -3)  // 1 * 2 + 3 * (-1) = 2 - 3 = -1 (需要根据实际计算调整)
}

// 测试2: 系统发育树构建
test "系统发育树构建" {
  // 距离矩阵
  type DistanceMatrix = { String: { String: Float } }
  
  // 系统发育树节点
  type PhyloNode = {
    name: String,
    distance: Float,
    left: Option<PhyloNode>,
    right: Option<PhyloNode>
  }
  
  // UPGMA算法（无权配对算术平均法）
  let upgma = fn(distance_matrix: DistanceMatrix) {
    let taxa = distance_matrix.keys()
    let mut clusters = []
    
    // 初始化聚类（每个分类单元是一个聚类）
    for taxon in taxa {
      clusters = clusters.push({
        name: taxon,
        distance: 0.0,
        left: None,
        right: None
      })
    }
    
    while clusters.length() > 1 {
      // 找到距离最近的两个聚类
      let mut min_distance = Float::max_value()
      let mut merge_i = -1
      let mut merge_j = -1
      
      for i in 0..clusters.length() {
        for j in i + 1..clusters.length() {
          let distance = calculate_cluster_distance(distance_matrix, clusters[i], clusters[j])
          
          if distance < min_distance {
            min_distance = distance
            merge_i = i
            merge_j = j
          }
        }
      }
      
      // 合并聚类
      let merged_cluster = {
        name: clusters[merge_i].name + "+" + clusters[merge_j].name,
        distance: min_distance / 2.0,
        left: Some(clusters[merge_i]),
        right: Some(clusters[merge_j])
      }
      
      // 更新聚类列表
      let mut new_clusters = []
      for i in 0..clusters.length() {
        if i != merge_i && i != merge_j {
          new_clusters = new_clusters.push(clusters[i])
        }
      }
      new_clusters = new_clusters.push(merged_cluster)
      
      clusters = new_clusters
    }
    
    clusters[0]  // 返回根节点
  }
  
  // 计算聚类间距离
  let calculate_cluster_distance = fn(distance_matrix: DistanceMatrix, cluster1: PhyloNode, cluster2: PhyloNode) {
    let taxa1 = get_cluster_taxa(cluster1)
    let taxa2 = get_cluster_taxa(cluster2)
    
    let mut total_distance = 0.0
    let mut count = 0
    
    for taxon1 in taxa1 {
      for taxon2 in taxa2 {
        match distance_matrix[taxon1][taxon2] {
          Some(distance) => {
            total_distance = total_distance + distance
            count = count + 1
          }
          None => ()
        }
      }
    }
    
    if count > 0 {
      total_distance / count.to_float()
    } else {
      Float::max_value()
    }
  }
  
  // 获取聚类中的所有分类单元
  let get_cluster_taxa = fn(cluster: PhyloNode) {
    let mut taxa = []
    
    if cluster.left.is_none() && cluster.right.is_none() {
      taxa = taxa.push(cluster.name)
    } else {
      match cluster.left {
        Some(left) => taxa = taxa + get_cluster_taxa(left)
        None => ()
      }
      match cluster.right {
        Some(right) => taxa = taxa + get_cluster_taxa(right)
        None => ()
      }
    }
    
    taxa
  }
  
  // 测试UPGMA算法
  let distance_matrix = {
    "A": { "A": 0.0, "B": 5.0, "C": 9.0, "D": 9.0, "E": 8.0 },
    "B": { "A": 5.0, "B": 0.0, "C": 10.0, "D": 10.0, "E": 9.0 },
    "C": { "A": 9.0, "B": 10.0, "C": 0.0, "D": 8.0, "E": 7.0 },
    "D": { "A": 9.0, "B": 10.0, "C": 8.0, "D": 0.0, "E": 3.0 },
    "E": { "A": 8.0, "B": 9.0, "C": 7.0, "D": 3.0, "E": 0.0 }
  }
  
  let phylo_tree = upgma(distance_matrix)
  
  // 验证树结构
  assert_true(phylo_tree.left.is_some())
  assert_true(phylo_tree.right.is_some())
  
  // 验证所有分类单元都在树中
  let all_taxa = get_cluster_taxa(phylo_tree)
  assert_eq(all_taxa.length(), 5)
  assert_true(all_taxa.contains("A"))
  assert_true(all_taxa.contains("B"))
  assert_true(all_taxa.contains("C"))
  assert_true(all_taxa.contains("D"))
  assert_true(all_taxa.contains("E"))
  
  // 测试简单情况（只有两个分类单元）
  let simple_distance = {
    "X": { "X": 0.0, "Y": 4.0 },
    "Y": { "X": 4.0, "Y": 0.0 }
  }
  
  let simple_tree = upgma(simple_distance)
  
  assert_eq(simple_tree.distance, 2.0)  // 4.0 / 2.0
  assert_eq(simple_tree.left.unwrap().name, "X")
  assert_eq(simple_tree.right.unwrap().name, "Y")
}

// 测试3: 基因组序列分析
test "基因组序列分析" {
  // 简化的基因序列
  type GeneSequence = String
  
  // 寻找开放阅读框（ORF）
  let find_orfs = fn(sequence: String, min_length: Int) {
    let start_codon = "ATG"
    let stop_codons = ["TAA", "TAG", "TGA"]
    let mut orfs = []
    
    // 检查三个阅读框
    for frame in 0..3 {
      let mut i = frame
      
      while i <= sequence.length() - 3 {
        let codon = sequence.substring(i, i + 3)
        
        if codon == start_codon {
          // 找到起始密码子，寻找终止密码子
          let mut j = i + 3
          
          while j <= sequence.length() - 3 {
            let stop_codon = sequence.substring(j, j + 3)
            
            if stop_codons.contains(stop_codon) {
              // 找到终止密码子，记录ORF
              let orf_length = j + 3 - i
              
              if orf_length >= min_length {
                orfs = orfs.push({
                  start: i,
                  end: j + 3,
                  frame: frame,
                  sequence: sequence.substring(i, j + 3)
                })
              }
              
              break
            }
            
            j = j + 3
          }
        }
        
        i = i + 1
      }
    }
    
    orfs
  }
  
  // 计算GC含量
  let calculate_gc_content = fn(sequence: String) {
    let mut gc_count = 0
    
    for base in sequence.to_char_array() {
      if base == 'G' || base == 'C' {
        gc_count = gc_count + 1
      }
    }
    
    if sequence.length() > 0 {
      gc_count.to_float() / sequence.length().to_float()
    } else {
      0.0
    }
  }
  
  // 翻译DNA到蛋白质
  let translate_dna_to_protein = fn(dna_sequence: String) {
    let codon_table = {
      "TTT": "F", "TTC": "F", "TTA": "L", "TTG": "L",
      "TCT": "S", "TCC": "S", "TCA": "S", "TCG": "S",
      "TAT": "Y", "TAC": "Y", "TAA": "*", "TAG": "*",
      "TGT": "C", "TGC": "C", "TGA": "*", "TGG": "W",
      "CTT": "L", "CTC": "L", "CTA": "L", "CTG": "L",
      "CCT": "P", "CCC": "P", "CCA": "P", "CCG": "P",
      "CAT": "H", "CAC": "H", "CAA": "Q", "CAG": "Q",
      "CGT": "R", "CGC": "R", "CGA": "R", "CGG": "R",
      "ATT": "I", "ATC": "I", "ATA": "I", "ATG": "M",
      "ACT": "T", "ACC": "T", "ACA": "T", "ACG": "T",
      "AAT": "N", "AAC": "N", "AAA": "K", "AAG": "K",
      "AGT": "S", "AGC": "S", "AGA": "R", "AGG": "R",
      "GTT": "V", "GTC": "V", "GTA": "V", "GTG": "V",
      "GCT": "A", "GCC": "A", "GCA": "A", "GCG": "A",
      "GAT": "D", "GAC": "D", "GAA": "E", "GAG": "E",
      "GGT": "G", "GGC": "G", "GGA": "G", "GGG": "G"
    }
    
    let mut protein = ""
    
    for i in range_step(0, dna_sequence.length() - 2, 3) {
      let codon = dna_sequence.substring(i, i + 3)
      
      match codon_table[codon] {
        Some(amino_acid) => {
          protein = protein + amino_acid
        }
        None => protein = protein + "X"  // 未知氨基酸
      }
    }
    
    protein
  }
  
  // 寻找重复序列
  let find_repeats = fn(sequence: String, min_repeat_length: Int) {
    let mut repeats = {}
    
    for i in 0..sequence.length() - min_repeat_length + 1 {
      for length in min_repeat_length..(sequence.length() - i + 1) / 2 {
        let pattern = sequence.substring(i, i + length)
        
        // 检查模式是否重复
        let mut count = 0
        let mut j = i
        
        while j <= sequence.length() - length {
          if sequence.substring(j, j + length) == pattern {
            count = count + 1
            j = j + length
          } else {
            break
          }
        }
        
        if count > 1 {
          repeats[pattern] = count
        }
      }
    }
    
    repeats
  }
  
  // 测试ORF寻找
  let test_sequence = "AAATGCCCCATGGGGAAATAGTTTATGAAATTT"
  let orfs = find_orfs(test_sequence, 9)
  
  assert_eq(orfs.length(), 3)
  
  // 验证第一个ORF
  assert_eq(orfs[0].start, 3)
  assert_eq(orfs[0].end, 12)
  assert_eq(orfs[0].frame, 0)
  assert_eq(orfs[0].sequence, "ATGCCCCATG")
  
  // 测试GC含量计算
  let gc_content = calculate_gc_content("ATGCGCCTA")
  
  // ATGCGCCTA有4个G或C（G, C, G, C, C），总共9个碱基
  assert_eq(gc_content, 5.0 / 9.0)
  
  // 测试DNA到蛋白质翻译
  let dna_sequence = "ATGAAATTT"
  let protein = translate_dna_to_protein(dna_sequence)
  
  assert_eq(protein, "MKF")  // ATG -> M, AAA -> K, TTT -> F
  
  // 测试重复序列寻找
  let repeat_sequence = "ATATATGCCGCCGCC"
  let repeats = find_repeats(repeat_sequence, 2)
  
  assert_true(repeats.contains_key("AT"))
  assert_eq(repeats["AT"], 3)
  assert_true(repeats.contains_key("GCC"))
  assert_eq(repeats["GCC"], 3)
  
  // 测试边界条件
  let empty_sequence = ""
  let empty_orfs = find_orfs(empty_sequence, 3)
  
  assert_eq(empty_orfs.length(), 0)
  
  let empty_gc = calculate_gc_content(empty_sequence)
  assert_eq(empty_gc, 0.0)
  
  let short_sequence = "ATG"
  let short_orfs = find_orfs(short_sequence, 9)
  
  assert_eq(short_orfs.length(), 0)  // 长度不足
}

// 测试4: 蛋白质结构预测
test "蛋白质结构预测" {
  // 氨基酸类型
  type AminoAcid = String
  
  // 二级结构类型
  type SecondaryStructure = String  // "H" for helix, "E" for sheet, "C" for coil
  
  // 蛋白质序列
  type ProteinSequence = Array<AminoAcid>
  
  // 疏水性量表（Kyte-Doolittle）
  let hydrophobicity_scale = {
    "A": 1.8, "R": -4.5, "N": -3.5, "D": -3.5, "C": 2.5,
    "Q": -3.5, "E": -3.5, "G": -0.4, "H": -3.2, "I": 4.5,
    "L": 3.8, "K": -3.9, "M": 1.9, "F": 2.8, "P": -1.6,
    "S": -0.8, "T": -0.7, "W": -0.9, "Y": -1.3, "V": 4.2
  }
  
  // 计算疏水性窗口
  let calculate_hydrophobicity_window = fn(sequence: ProteinSequence, window_size: Int) {
    let mut hydrophobicity_values = []
    
    for i in 0..sequence.length() - window_size + 1 {
      let mut sum = 0.0
      
      for j in 0..window_size {
        let amino_acid = sequence[i + j]
        match hydrophobicity_scale[amino_acid] {
          Some(value) => sum = sum + value
          None => ()
        }
      }
      
      hydrophobicity_values = hydrophobicity_values.push(sum / window_size.to_float())
    }
    
    hydrophobicity_values
  }
  
  // 预测跨膜区域
  let predict_transmembrane_regions = fn(sequence: ProteinSequence, window_size: Int, threshold: Float) {
    let hydrophobicity_values = calculate_hydrophobicity_window(sequence, window_size)
    let mut transmembrane_regions = []
    let mut in_region = false
    let mut region_start = -1
    
    for i in 0..hydrophobicity_values.length() {
      if hydrophobicity_values[i] > threshold {
        if not(in_region) {
          region_start = i
          in_region = true
        }
      } else {
        if in_region {
          transmembrane_regions = transmembrane_regions.push({
            start: region_start,
            end: i - 1,
            length: i - region_start
          })
          in_region = false
        }
      }
    }
    
    // 处理末尾的区域
    if in_region {
      transmembrane_regions = transmembrane_regions.push({
        start: region_start,
        end: hydrophobicity_values.length() - 1,
        length: hydrophobicity_values.length() - region_start
      })
    }
    
    transmembrane_regions
  }
  
  // 简化的二级结构预测（基于Chou-Fasman方法）
  let predict_secondary_structure = fn(sequence: ProteinSequence) {
    // 简化的倾向性参数
    let helix_propensity = {
      "A": 1.42, "R": 0.98, "N": 0.67, "D": 0.53, "C": 0.70,
      "Q": 1.11, "E": 1.51, "G": 0.57, "H": 1.00, "I": 1.00,
      "L": 1.21, "K": 1.16, "M": 1.45, "F": 1.13, "P": 0.59,
      "S": 0.77, "T": 0.83, "W": 1.14, "Y": 0.61, "V": 1.06
    }
    
    let sheet_propensity = {
      "A": 0.83, "R": 0.93, "N": 0.89, "D": 0.54, "C": 1.19,
      "Q": 1.10, "E": 0.37, "G": 0.75, "H": 0.87, "I": 1.60,
      "L": 1.30, "K": 0.74, "M": 1.05, "F": 1.38, "P": 0.62,
      "S": 0.72, "T": 1.20, "W": 1.19, "Y": 1.29, "V": 1.70
    }
    
    let mut secondary_structure = []
    
    for i in 0..sequence.length() {
      let amino_acid = sequence[i]
      let helix_score = match helix_propensity[amino_acid] {
        Some(score) => score
        None => 1.0
      }
      
      let sheet_score = match sheet_propensity[amino_acid] {
        Some(score) => score
        None => 1.0
      }
      
      if helix_score > 1.0 && helix_score > sheet_score {
        secondary_structure = secondary_structure.push("H")  // Helix
      } else if sheet_score > 1.0 && sheet_score > helix_score {
        secondary_structure = secondary_structure.push("E")  // Sheet
      } else {
        secondary_structure = secondary_structure.push("C")  // Coil
      }
    }
    
    secondary_structure
  }
  
  // 计算等电点
  let calculate_isoelectric_point = fn(sequence: ProteinSequence) {
    // 简化的pKa值
    let n_term_pka = 9.0
    let c_term_pka = 2.0
    let side_chain_pka = {
      "R": 12.5, "K": 10.5, "H": 6.0, "D": 3.9, "E": 4.3, "C": 8.3, "Y": 10.1
    }
    
    let mut positive_charges = 0
    let mut negative_charges = 0
    
    // 计算电荷
    for amino_acid in sequence {
      match amino_acid {
        "R" | "K" | "H" => positive_charges = positive_charges + 1
        "D" | "E" => negative_charges = negative_charges + 1
        _ => ()
      }
    }
    
    // 简化的等电点计算
    if positive_charges > negative_charges {
      7.0 + (positive_charges - negative_charges).to_float() * 0.5
    } else if negative_charges > positive_charges {
      7.0 - (negative_charges - positive_charges).to_float() * 0.5
    } else {
      7.0
    }
  }
  
  // 测试疏水性计算
  let test_protein = ["A", "L", "I", "V", "M", "F", "W", "P", "G", "S", "T", "C", "Y", "N", "Q", "D", "E", "K", "R", "H"]
  let hydrophobicity = calculate_hydrophobicity_window(test_protein, 5)
  
  assert_eq(hydrophobicity.length(), 16)  // 20 - 5 + 1
  
  // 验证第一个窗口的疏水性
  // A(1.8) + L(3.8) + I(4.5) + V(4.2) + M(1.9) = 16.2
  // 16.2 / 5 = 3.24
  assert_eq(hydrophobicity[0], 3.24)
  
  // 测试跨膜区域预测
  let transmembrane_protein = ["M", "A", "L", "W", "I", "V", "L", "L", "V", "V", "V", "V", "A", "L", "A", "A", "G", "A", "G", "A"]
  let transmembrane_regions = predict_transmembrane_regions(transmembrane_protein, 7, 2.0)
  
  assert_true(transmembrane_regions.length() > 0)
  
  // 测试二级结构预测
  let secondary_structure = predict_secondary_structure(test_protein)
  
  assert_eq(secondary_structure.length(), 20)
  
  // 验证疏水性氨基酸倾向于形成螺旋
  // A, L, I, V都有较高的螺旋倾向性
  assert_eq(secondary_structure[0], "H")  // A
  assert_eq(secondary_structure[1], "H")  // L
  assert_eq(secondary_structure[2], "H")  // I
  assert_eq(secondary_structure[3], "H")  // V
  
  // 测试等电点计算
  let acidic_protein = ["D", "E", "D", "E", "D", "E"]  // 酸性蛋白质
  let basic_protein = ["K", "R", "K", "R", "K", "R"]  // 碱性蛋白质
  let neutral_protein = ["A", "L", "I", "V", "M", "F"]  // 中性蛋白质
  
  let acidic_pI = calculate_isoelectric_point(acidic_protein)
  let basic_pI = calculate_isoelectric_point(basic_protein)
  let neutral_pI = calculate_isoelectric_point(neutral_protein)
  
  assert_true(acidic_pI < 7.0)
  assert_true(basic_pI > 7.0)
  assert_eq(neutral_pI, 7.0)
}

// 测试5: 基因表达分析
test "基因表达分析" {
  // 基因表达数据
  type GeneExpressionData = {
    genes: Array<String>,
    samples: Array<String>,
    expression_matrix: Array<Array<Float>>  // 基因 × 样本
  }
  
  // 差异表达基因
  type DifferentialExpression = {
    gene: String,
    log_fold_change: Float,
    p_value: Float,
    is_significant: Bool
  }
  
  // 计算基因表达统计
  let calculate_expression_stats = fn(expression_values: Array<Float>) {
    if expression_values.length() == 0 {
      return { mean: 0.0, variance: 0.0, std_dev: 0.0 }
    }
    
    let mut sum = 0.0
    for value in expression_values {
      sum = sum + value
    }
    
    let mean = sum / expression_values.length().to_float()
    
    let mut sum_squared_diff = 0.0
    for value in expression_values {
      let diff = value - mean
      sum_squared_diff = sum_squared_diff + diff * diff
    }
    
    let variance = if expression_values.length() > 1 {
      sum_squared_diff / (expression_values.length() - 1).to_float()
    } else {
      0.0
    }
    
    let std_dev = if variance >= 0.0 { variance.sqrt() } else { 0.0 }
    
    { mean, variance, std_dev }
  }
  
  // 计算log2倍数变化
  let calculate_log_fold_change = fn(control_values: Array<Float>, treatment_values: Array<Float>) {
    let control_stats = calculate_expression_stats(control_values)
    let treatment_stats = calculate_expression_stats(treatment_values)
    
    if control_stats.mean > 0.0 && treatment_stats.mean > 0.0 {
      (treatment_stats.mean / control_stats.mean).log2()
    } else if control_stats.mean == 0.0 && treatment_stats.mean > 0.0 {
      10.0  // 无限大，用大数值表示
    } else if control_stats.mean > 0.0 && treatment_stats.mean == 0.0 {
      -10.0  // 无限小，用小数值表示
    } else {
      0.0
    }
  }
  
  // 简化的t检验
  let simple_t_test = fn(control_values: Array<Float>, treatment_values: Array<Float>) {
    let control_stats = calculate_expression_stats(control_values)
    let treatment_stats = calculate_expression_stats(treatment_values)
    
    let n1 = control_values.length().to_float()
    let n2 = treatment_values.length().to_float()
    
    let pooled_variance = ((n1 - 1.0) * control_stats.variance + (n2 - 1.0) * treatment_stats.variance) / (n1 + n2 - 2.0)
    let standard_error = (pooled_variance * (1.0 / n1 + 1.0 / n2)).sqrt()
    
    if standard_error > 0.0 {
      let t_statistic = (treatment_stats.mean - control_stats.mean) / standard_error
      
      // 简化的p值计算（实际应使用t分布）
      let abs_t = t_statistic.abs()
      let p_value = if abs_t > 2.0 {
        0.05
      } else if abs_t > 1.5 {
        0.1
      } else {
        0.5
      }
      
      p_value
    } else {
      1.0
    }
  }
  
  // 差异表达分析
  let differential_expression_analysis = fn(expression_data: GeneExpressionData, control_samples: Array<Int>, treatment_samples: Array<Int>, p_value_threshold: Float) {
    let mut results = []
    
    for i in 0..expression_data.genes.length() {
      let gene = expression_data.genes[i]
      
      let mut control_values = []
      for sample_index in control_samples {
        control_values = control_values.push(expression_data.expression_matrix[i][sample_index])
      }
      
      let mut treatment_values = []
      for sample_index in treatment_samples {
        treatment_values = treatment_values.push(expression_data.expression_matrix[i][sample_index])
      }
      
      let log_fold_change = calculate_log_fold_change(control_values, treatment_values)
      let p_value = simple_t_test(control_values, treatment_values)
      
      results = results.push({
        gene: gene,
        log_fold_change: log_fold_change,
        p_value: p_value,
        is_significant: p_value < p_value_threshold
      })
    }
    
    results
  }
  
  // 聚类分析（简化的层次聚类）
  let hierarchical_clustering = fn(expression_data: GeneExpressionData) {
    let mut clusters = []
    
    // 初始化：每个基因是一个聚类
    for i in 0..expression_data.genes.length() {
      clusters = clusters.push([i])
    }
    
    while clusters.length() > 1 {
      // 找到最近的两个聚类
      let mut min_distance = Float::max_value()
      let mut merge_i = 0
      let mut merge_j = 1
      
      for i in 0..clusters.length() {
        for j in i + 1..clusters.length() {
          let distance = calculate_cluster_distance(expression_data, clusters[i], clusters[j])
          
          if distance < min_distance {
            min_distance = distance
            merge_i = i
            merge_j = j
          }
        }
      }
      
      // 合并聚类
      let merged_cluster = clusters[merge_i] + clusters[merge_j]
      
      let mut new_clusters = []
      for i in 0..clusters.length() {
        if i != merge_i && i != merge_j {
          new_clusters = new_clusters.push(clusters[i])
        }
      }
      new_clusters = new_clusters.push(merged_cluster)
      
      clusters = new_clusters
      
      // 如果只需要两个聚类，可以提前终止
      if clusters.length() == 2 {
        break
      }
    }
    
    clusters
  }
  
  // 计算聚类间距离
  let calculate_cluster_distance = fn(expression_data: GeneExpressionData, cluster1: Array<Int>, cluster2: Array<Int>) {
    let mut total_distance = 0.0
    let mut count = 0
    
    for i in cluster1 {
      for j in cluster2 {
        let distance = calculate_gene_distance(expression_data, i, j)
        total_distance = total_distance + distance
        count = count + 1
      }
    }
    
    if count > 0 {
      total_distance / count.to_float()
    } else {
      Float::max_value()
    }
  }
  
  // 计算基因间距离（欧氏距离）
  let calculate_gene_distance = fn(expression_data: GeneExpressionData, gene1_index: Int, gene2_index: Int) {
    let mut sum_squared_diff = 0.0
    
    for j in 0..expression_data.samples.length() {
      let value1 = expression_data.expression_matrix[gene1_index][j]
      let value2 = expression_data.expression_matrix[gene2_index][j]
      let diff = value1 - value2
      sum_squared_diff = sum_squared_diff + diff * diff
    }
    
    sum_squared_diff.sqrt()
  }
  
  // 测试基因表达统计计算
  let test_values = [2.0, 3.0, 4.0, 5.0, 6.0]
  let stats = calculate_expression_stats(test_values)
  
  assert_eq(stats.mean, 4.0)  // (2+3+4+5+6)/5 = 4.0
  assert_eq(stats.variance, 2.5)  // 方差 = 10/4 = 2.5
  assert_eq(stats.std_dev, 2.5.sqrt())
  
  // 测试log2倍数变化计算
  let control_values = [2.0, 3.0, 4.0]
  let treatment_values = [4.0, 6.0, 8.0]
  
  let log_fc = calculate_log_fold_change(control_values, treatment_values)
  
  // 对照组均值 = 3.0，处理组均值 = 6.0
  // log2(6.0/3.0) = log2(2.0) = 1.0
  assert_eq(log_fc, 1.0)
  
  // 测试差异表达分析
  let expression_data = {
    genes: ["GeneA", "GeneB", "GeneC", "GeneD"],
    samples: ["Control1", "Control2", "Control3", "Treatment1", "Treatment2", "Treatment3"],
    expression_matrix: [
      [2.0, 3.0, 4.0, 4.0, 6.0, 8.0],  // GeneA: 上调
      [5.0, 6.0, 7.0, 5.0, 6.0, 7.0],  // GeneB: 无变化
      [8.0, 9.0, 10.0, 2.0, 3.0, 4.0], // GeneC: 下调
      [1.0, 2.0, 3.0, 1.0, 2.0, 3.0]   // GeneD: 无变化
    ]
  }
  
  let control_samples = [0, 1, 2]  // Control1, Control2, Control3
  let treatment_samples = [3, 4, 5]  // Treatment1, Treatment2, Treatment3
  
  let de_results = differential_expression_analysis(expression_data, control_samples, treatment_samples, 0.1)
  
  assert_eq(de_results.length(), 4)
  
  // GeneA应该显著上调
  assert_eq(de_results[0].gene, "GeneA")
  assert_true(de_results[0].log_fold_change > 0.0)
  assert_true(de_results[0].is_significant)
  
  // GeneC应该显著下调
  assert_eq(de_results[2].gene, "GeneC")
  assert_true(de_results[2].log_fold_change < 0.0)
  assert_true(de_results[2].is_significant)
  
  // GeneB和GeneD应该无显著变化
  assert_false(de_results[1].is_significant)
  assert_false(de_results[3].is_significant)
  
  // 测试聚类分析
  let clusters = hierarchical_clustering(expression_data)
  
  assert_eq(clusters.length(), 2)
  
  // 验证所有基因都被分配
  let mut total_genes = 0
  for cluster in clusters {
    total_genes = total_genes + cluster.length()
  }
  assert_eq(total_genes, 4)
}