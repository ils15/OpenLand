# Teste rápido da funcionalidade exclude_classes com suporte terra/raster
# Este script verifica se a implementação está funcionando

# Carregar os pacotes necessários
library(OpenLand)

# Verificar se terra está disponível
terra_available <- requireNamespace("terra", quietly = TRUE)
raster_available <- requireNamespace("raster", quietly = TRUE)

if (!raster_available) {
  stop("Pacote 'raster' é necessário para este teste")
}

library(raster)
if (terra_available) {
  library(terra)
}

cat("Testando a funcionalidade exclude_classes...\n")
cat("Terra disponível:", terra_available, "\n")
cat("Raster disponível:", raster_available, "\n\n")

# Criar rasters de teste simples usando raster
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

cat("1. Testando com objetos raster:\n")

# Teste com raster
tryCatch({
  result_raster <- contingencyTable(test_stack, pixelresolution = 1, exclude_classes = 0)
  cat("✓ Sucesso com raster\n")
  cat("Classes presentes:", paste(sort(unique(result_raster$tb_legend$categoryValue)), collapse = ", "), "\n")
}, error = function(e) {
  cat("✗ Erro com raster:", e$message, "\n")
})

# Teste com terra se disponível
if (terra_available) {
  cat("\n2. Testando com objetos terra:\n")
  
  tryCatch({
    # Converter para terra
    terra_r1 <- terra::rast(r1)
    terra_r2 <- terra::rast(r2)
    names(terra_r1) <- "landscape_2020"
    names(terra_r2) <- "landscape_2021"
    terra_stack <- c(terra_r1, terra_r2)
    
    result_terra <- contingencyTable(terra_stack, pixelresolution = 1, exclude_classes = 0)
    cat("✓ Sucesso com terra\n")
    cat("Classes presentes:", paste(sort(unique(result_terra$tb_legend$categoryValue)), collapse = ", "), "\n")
  }, error = function(e) {
    cat("✗ Erro com terra:", e$message, "\n")
  })
} else {
  cat("\n2. Terra não disponível - pulando teste terra\n")
}

cat("\nTeste concluído!\n")
