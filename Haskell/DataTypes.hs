{-# LANGUAGE DeriveGeneric #-}

module DataTypes(
   Transacao(..),
   GregorianCalendar(..),
   TipoTransacao(..)
   )
   where
   
   import Data.Aeson
   import GHC.Generics
   
   data Transacao = 
    Transacao{
       datas :: GregorianCalendar,
       valor :: Double,
       textoIdentificador :: String,
       descricao :: String,
       numeroDOC :: String,
       tipos :: [TipoTransacao]
    } deriving (Generic, Show)
   
   data GregorianCalendar =
    GregorianCalendar {
       year :: Int,
       month :: Int,
       dayOfMonth :: Int
    } deriving (Generic, Show)
   
   data TipoTransacao = 
      SALDO_CORRENTE 
      | VALOR_APLICACAO 
      | RECEITA_OPERACIONAL 
      | TAXA_CONDOMINIO 
      | TAXA_EXTRA 
      | TAXA_SALAO_FESTA 
      | MULTAS_JUROS 
      | TAXA_AGUA 
      | RECUPERACAO_ATIVOS 
      | MULTA_JURO_CORRECAO_COBRANCA 
      | OUTRAS_RECEITAS 
      | DESPESAS_PESSOAL 
      | TERCEIRIZACAO_FUNCIONARIOS 
      | VIGILANCIA 
      | SALARIO_FUNCIONARIOS_ORGANICOS 
      | ADIANTAMENTO_SALARIAL_FUNCIONARIOS_ORGANICOS 
      | FERIAS 
      | INSS 
      | FGTS 
      | PIS  
      | ISS  
      | BENEFICIO_SOCIAL 
      | OUTRAS_DESPESAS_PESSOAL 
      | DESPESAS_ADMINISTRATIVAS 
      | ENERGISA 
      | CAGEPA
      | COMPRA
      | ADMINISTRACAO_CONDOMINIO 
      | MANUTENCAO 
      | ABASTECIMENTO 
      | SERVICOS_TERCEIROS 
      | IRPF 
      | TARIFAS_BANCARIAS 
      | OUTRAS_DESPESAS_ADMINISTRATIVAS 
      | APLICACAO 
      | OUTROS deriving (Eq, Enum, Generic)
   
   instance Show TipoTransacao where
      show SALDO_CORRENTE = "Saldo Corrente" 
      show VALOR_APLICACAO = "Valor aplicado"
      show RECEITA_OPERACIONAL = "Receita Operacional" 
      show TAXA_CONDOMINIO = "Taxas de Condominio"
      show TAXA_EXTRA = "Taxas Extras"
      show TAXA_SALAO_FESTA = "Taxas Salao de Festas"
      show MULTAS_JUROS = "Multas e Juros"
      show TAXA_AGUA = "Taxa de Agua"
      show RECUPERACAO_ATIVOS = "Recuperacao de Ativos"
      show MULTA_JURO_CORRECAO_COBRANCA = "Multa, Juros e Correcao de Cobranca"
      show OUTRAS_RECEITAS = "Outras receitas"
      show DESPESAS_PESSOAL = "Despesas com pessoal"
      show TERCEIRIZACAO_FUNCIONARIOS = "Terceirizacao de Funcionarios"
      show VIGILANCIA = "Vigilancia"
      show SALARIO_FUNCIONARIOS_ORGANICOS = "Salario dos funcionarios organicos"
      show ADIANTAMENTO_SALARIAL_FUNCIONARIOS_ORGANICOS = "Adiantamento salarial dos funcionarios organicos"
      show FERIAS = "Ferias"
      show INSS = "INSS funcionarios e vigilancia"
      show FGTS = "FGTS"
      show PIS = "PIS"
      show ISS = "ISS"
      show BENEFICIO_SOCIAL = "Beneficio social dos funcionarios organicos"
      show OUTRAS_DESPESAS_PESSOAL = "Outras despesas com pessoal"
      show DESPESAS_ADMINISTRATIVAS = "Despesas Administrativas"
      show ENERGISA = "Energisa"
      show CAGEPA = "CAGEPA"
      show COMPRA = "Compra"
      show ADMINISTRACAO_CONDOMINIO = "Administracao do condominio"
      show MANUTENCAO = "Manutencao realizada"
      show ABASTECIMENTO = "Abastecimentos"
      show SERVICOS_TERCEIROS = "Servicos realizados por terceiros"
      show IRPF = "Imposto de renda recolhido na fonte"
      show TARIFAS_BANCARIAS = "Tarifas e taxas bancarias"
      show OUTRAS_DESPESAS_ADMINISTRATIVAS = "Outras despesas administrativas"
      show APLICACAO = "Aplicacao"
      show OUTROS = "Outros"
   
   -- Declarado conforme documentacao do Aeson (https://artyom.me/aeson#records-and-json-generics)
   -- Outros exemplos, caso não rode: https://dev.to/piq9117/haskell-encoding-and-decoding-json-with-aeson-5d7n
   instance ToJSON Transacao
   instance FromJSON Transacao where 

 
   instance ToJSON GregorianCalendar
   instance FromJSON GregorianCalendar
  
   instance ToJSON TipoTransacao
   instance FromJSON TipoTransacao

   instance Eq Transacao where
      Transacao _ x _ _ _ _ == Transacao _ y _ _ _ _  = x == y
   


