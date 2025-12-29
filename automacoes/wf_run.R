# meu_script.R

# 1. Capturar todos os argumentos da linha de comando
# O parâmetro 'trailingOnly = TRUE' garante que apenas os argumentos
# que vêm APÓS o nome do script serão capturados.
args <- commandArgs(trailingOnly = TRUE)

# 2. Verificação básica de argumentos
if (length(args) != 2) {
  stop("É necessário fornecer exatamente 2 argumentos (os números a serem somados).", call. = FALSE)
}

# 3. Converter os argumentos (que são strings) para números
# Usamos as.numeric para garantir que são interpretados como números
num1 <- as.numeric(args[1])
num2 <- as.numeric(args[2])

# 4. Verificar se a conversão foi bem-sucedida (se são números válidos)
if (is.na(num1) || is.na(num2)) {
  stop("Os argumentos devem ser números válidos.", call. = FALSE)
}

# 5. Lógica principal do script
soma <- num1 + num2

# 6. Imprimir o resultado
cat("O primeiro número é:", num1, "\n")
cat("O segundo número é:", num2, "\n")
cat("A soma é:", soma, "\n")

# Para garantir que o script saia corretamentewf_run
# Para scripts simples, não é estritamente necessário, mas é uma boa prática.
# q(save = "no")