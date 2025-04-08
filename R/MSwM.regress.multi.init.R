#' Estime un modele de regression a changement de regime markovien avec initialisations multiples
#'
#' Cette fonction etend MSwM::msmFit en testant plusieurs initialisations aleatoires
#' pour trouver la solution optimale du modele de regression a changement de regime.
#'
#' @param lm_model Un objet de classe 'lm' representant le modele lineaire initial
#' @param sw_vector Vecteur logique indiquant les parametres qui changent entre regimes
#' @param n_regimes Entier. Le nombre de regimes souhaite (par defaut: 2)
#' @param seeds Vecteur d'entiers ou entier unique. Les seeds pour les initialisations
#'              aleatoires multiples. Si un entier unique est fourni, il sera utilise pour
#'              generer un vecteur 1:seeds (par defaut: 1:10)
#' @param p_order Entier. L'ordre du processus autoregressif (par defaut: 1)
#' @param control_params Liste. Parametres de controle pour l'estimation:
#'        \itemize{
#'          \item parallelization: Booleen. Active la parallelisation (defaut: FALSE)
#'          \item tol: Numerique. Tolerance pour la convergence (defaut: 1e-20)
#'          \item maxiter: Entier. Nombre maximum d'iterations totales (defaut: 1000)
#'          \item maxiterOuter: Entier. Nombre maximum d'iterations externes (defaut: 50)
#'          \item maxiterInner: Entier. Nombre maximum d'iterations internes (defaut: 10)
#'          \item trace: Booleen. Affiche les details d'estimation (defaut: FALSE)
#'        }
#'
#' @return Une liste contenant :
#'         \itemize{
#'           \item model: Le meilleur modele estime (objet de classe 'msmFit')
#'           \item seed: La seed ayant produit le meilleur modele
#'           \item aic: La valeur AIC du meilleur modele
#'         }
#'
#' @export
#'
#' @details
#' La fonction utilise MSwM::msmFit avec plusieurs initialisations aleatoires
#' pour eviter les optimums locaux. Elle teste systematiquement differentes valeurs
#' initiales et selectionne le modele ayant le meilleur critere AIC.
#'
#' @examples
#' # Modele lineaire initial
#' \dontrun{
#' lm_init <- lm(y ~ x1 + x2, data = my_data)
#'
#' # Estimation avec initialisations multiples
#' ms_model <- MSwM.regress.multi.init(lm_model = lm_init, n_regimes = 2, seeds = 1:20, p_order = 1)
#'
#' # Affichage des resultats
#' summary(ms_model$model)
#'}
#' @import MSwM
#'
#'
MSwM.regress.multi.init <- function(lm_model,
                                    n_regimes = 2,
                                    sw_vector = rep(TRUE, length(stats::coef(lm_model)) + 2),
                                    seeds = 1:10,
                                    p_order = 1,
                                    control_params = list(
                                      parallelization = FALSE,
                                      tol = 1e-20,
                                      maxiter = 1000,
                                      maxiterOuter = 50,
                                      maxiterInner = 10,
                                      trace = FALSE
                                    )) {

  # Verification du modele initial
  if (!inherits(lm_model, "lm")) stop("Le modele initial doit etre un objet 'lm'")

  # Conversion de seeds en vecteur si necessaire
  if (length(seeds) == 1) seeds <- 1:seeds

  # Initialisation
  best_model <- NULL
  best_aic <- Inf
  best_seed <- NULL

  # Nombre total d'iterations pour l'affichage
  total_iterations <- ifelse(length(seeds) == 1, max(seeds), length(seeds))

  # Estimation avec differentes initialisations
  for (seed in seeds) {
    cat(sprintf("Seed %d/%d\n", seed, total_iterations))

    set.seed(seed)
    current_model <- try({
      MSwM::msmFit(lm_model,
                   k = n_regimes,
                   sw = sw_vector,
                   p = p_order,
                   control = control_params)
    }, silent = TRUE)

    if (!inherits(current_model, "try-error")) {
      current_aic <- MSwM::AIC(current_model)

      if (current_aic < best_aic) {
        best_aic <- current_aic
        best_model <- current_model
        best_seed <- seed
        cat(sprintf("Nouveau meilleur modele (seed %d), AIC: %.4f\n", seed, best_aic))
      }
    } else {
      cat(sprintf("Echec de l'estimation avec seed %d\n", seed))
    }
  }

  if (is.null(best_model)) stop("Aucun modele n'a converge")

  # Resume du meilleur modele
  cat("\nMeilleur modele trouve:\n")
  cat(sprintf("Seed: %d\n", best_seed))
  cat(sprintf("AIC: %.4f\n\n", best_aic))
  summary(best_model)

  # Retourne une liste avec les elements importants
  return(list(
    model = best_model,
    seed = best_seed,
    aic = best_aic
  ))
}
