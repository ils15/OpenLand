# Teste rápido da funcionalidade exclude_classes
# Este script verifica se a implementação está funcionando

# Carregar o pacote
library(OpenLand)
library(raster)

# Criar rasters de teste simples
test_matrix_1 <- matrix(c(0, 0, 1, 1, 
                         2, 2, 0, 1, 
                         2, 0, 1, 2,
                         1, 2, 0, 1), nrow = 4)

test_matrix_2 <- matrix(c(0, 1, 1, 2, 
                         2, 0, 1, 2, 
                         0, 1, 2, 0,
                         2, 1, 0, 2), nrow = 4)

r1 <- raster(test_matrix_1)
r2 <- raster(test_matrix_2)

names(r1) <- "landscape_2020"
names(r2) <- "landscape_2021"

test_stack <- stack(r1, r2)

cat("Testando a funcionalidade exclude_classes...\n\n")

# Teste 1: Sem exclusões
cat("1. Análise sem exclusões:\n")
result_normal <- contingencyTable(test_stack, pixelresolution = 1)
cat("Classes presentes:", paste(sort(unique(result_normal$tb_legend$categoryValue)), collapse = ", "), "\n")
cat("Número de transições:", nrow(result_normal$lulc_Multistep), "\n\n")

# Teste 2: Excluindo classe 0
cat("2. Análise excluindo classe 0:\n")
result_no_zero <- contingencyTable(test_stack, pixelresolution = 1, exclude_classes = 0)
cat("Classes presentes:", paste(sort(unique(result_no_zero$tb_legend$categoryValue)), collapse = ", "), "\n")
cat("Número de transições:", nrow(result_no_zero$lulc_Multistep), "\n")
cat("Classes excluídas:", paste(attr(result_no_zero$tb_legend, "excluded_classes"), collapse = ", "), "\n\n")

# Teste 3: Excluindo múltiplas classes
cat("3. Análise excluindo classes 0 e 2:\n")
result_multiple <- contingencyTable(test_stack, pixelresolution = 1, exclude_classes = c(0, 2))
cat("Classes presentes:", paste(sort(unique(result_multiple$tb_legend$categoryValue)), collapse = ", "), "\n")
cat("Número de transições:", nrow(result_multiple$lulc_Multistep), "\n")
cat("Classes excluídas:", paste(attr(result_multiple$tb_legend, "excluded_classes"), collapse = ", "), "\n\n")

# Verificação
cat("4. Verificação:\n")
has_zero_in_filtered <- any(c(0) %in% c(result_no_zero$lulc_Multistep$From, result_no_zero$lulc_Multistep$To))
has_zero_or_two_in_multiple <- any(c(0, 2) %in% c(result_multiple$lulc_Multistep$From, result_multiple$lulc_Multistep$To))

if (!has_zero_in_filtered) {
  cat("✓ Exclusão da classe 0 funcionou corretamente\n")
} else {
  cat("✗ Erro: Classe 0 ainda aparece nos resultados filtrados\n")
}

if (!has_zero_or_two_in_multiple) {
  cat("✓ Exclusão múltipla (0 e 2) funcionou corretamente\n")
} else {
  cat("✗ Erro: Classes 0 ou 2 ainda aparecem nos resultados com exclusão múltipla\n")
}

cat("\nTeste concluído!\n")
