# Propensão à subscrição de depósito a prazo — Regressão Logística

Modelo de regressão logística que estima a probabilidade de um cliente de um banco português subscrever um depósito a prazo após uma campanha de marketing por telefone. O projeto percorre o fluxo completo: análise descritiva, análise bivariada, preparação de variáveis, seleção de modelos por stepwise e avaliação sob três objetivos (estimação, decisão e ordenação).

## Dados

`bank.csv` — [Bank Marketing](https://archive.ics.uci.edu/dataset/222/bank+marketing), UCI Machine Learning Repository (licença CC BY 4.0).

- 4.521 clientes, 16 variáveis explicativas, separador `;`
- Variável resposta `y`: o cliente subscreveu (`yes`) ou não (`no`) o depósito, com cerca de 11,5% de `yes`

> Moro, S., Laureano, R. & Cortez, P. (2011). *Using Data Mining for Bank Direct Marketing: An Application of the CRISP-DM Methodology*. Proceedings of the European Simulation and Modelling Conference — ESM'2011, pp. 117-121.

## Estrutura

| Arquivo | Conteúdo |
|---|---|
| `bank_model.ipynb` | Versão principal em Python, com análise, código e interpretação de cada etapa |
| `Bank_model.R` | Versão original do trabalho, em R |
| `bank.csv` | Base de dados |

## Metodologia (notebook)

1. **Qualidade dos dados:** valores faltantes e categorias `unknown`.
2. **Análise univariada:** distribuições e frequências de cada variável.
3. **Análise bivariada:**
   - Teste de Mann-Whitney para as variáveis numéricas.
   - Verificação da linearidade com o logito por faixas.
   - Qui-quadrado e V de Cramér para as variáveis categóricas.
4. **Preparação:**
   - Divisão treino/teste (70/30), estratificada por `y`.
   - Categorização de `age`, `balance`, `day`, `job` e `month`.
   - Dummies com a categoria mais frequente como referência.
5. **Modelagem:** regressão logística (`statsmodels`) com stepwise bidirecional por AIC e por p-valor (penalidade 3,84, o quantil 95% da χ² com 1 grau de liberdade), além de VIF e odds ratios.
6. **Avaliação na base de teste:**
   - Estimação: MSE (Brier) e RMSE.
   - Decisão: precision, recall e F1 em vários pontos de corte, incluindo o de máximo KS.
   - Ordenação: AUC, KS, lift por decil, ganho acumulado e curva ROC.

### Decisões importantes

- **`duration` fica fora dos modelos principais.** A duração da ligação só é conhecida depois que ela termina, ou seja, quando o resultado já é conhecido. Ela aparece apenas num modelo de referência, para mostrar o quanto infla as métricas.
- **`pdays` e `previous` são removidas**, porque a informação delas já está em `poutcome`.
- **Avaliação fora da amostra:** todas as escolhas aprendidas dos dados (mediana de `balance`, categorias de referência, coeficientes e ponto de corte) usam somente a base de treino.

## Diferenças em relação à versão em R

| Versão em R | Notebook Python |
|---|---|
| Ajuste e avaliação na mesma base | Divisão treino/teste estratificada |
| `duration` nos modelos | `duration` só no modelo de referência |
| Cortes 0,4 / 0,5 / 0,6 | Cortes de 0,1 a 0,6, além do corte de máximo KS |
| Gráfico de proporção por número do percentil (30 faixas) | Logito por valor médio da faixa (10 faixas) |
| KS aproximado por decis | KS exato pela curva ROC |

## Como executar

Requer Python 3.10 ou superior.

```bash
python -m venv .venv
# Windows: .venv\Scripts\activate    |    Linux/macOS: source .venv/bin/activate
pip install -r requirements.txt
jupyter notebook bank_model.ipynb
```

Para a versão em R, abra `Bank_model.R` no RStudio com o diretório de trabalho na pasta do repositório (o `bank.csv` é lido por caminho relativo). Os pacotes usados são readr, tidyverse, car, fastDummies, rcompanion e pROC.

## Licença

Código sob licença [MIT](LICENSE). Os dados (`bank.csv`) pertencem aos autores originais e são distribuídos sob CC BY 4.0.
