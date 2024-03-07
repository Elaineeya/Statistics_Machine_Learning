import numpy as np

def calcAccuracy(LPred, LTrue):
    """Calculates prediction accuracy from data labels.

    Args:
        LPred (array): Predicted data labels.
        LTrue (array): Ground truth data labels.

    Retruns:
        acc (float): Prediction accuracy.
    """

    # --------------------------------------------
    # === Your code here =========================
    # --------------------------------------------
    acc = sum(LPred == LTrue) / len(LTrue)
    # ============================================
    return acc


def calcConfusionMatrix(LPred, LTrue):
    """Calculates a confusion matrix from data labels.

    Args:
        LPred (array): Predicted data labels.
        LTrue (array): Ground truth data labels.

    Returns:
        cM (array): Confusion matrix, with predicted labels in the rows
            and actual labels in the columns.
    """

    # --------------------------------------------
    # === Your code here =========================
    # --------------------------------------------
    unique_labels = np.unique(np.concatenate((LPred, LTrue)))
    num_labels = len(unique_labels)
    cM = np.zeros((num_labels, num_labels), dtype=int)
    for pred_label, true_label in zip(LPred, LTrue):
        true_index = np.where(unique_labels == true_label)[0][0]
        pred_index = np.where(unique_labels == pred_label)[0][0]
        cM[pred_index, true_index] += 1
    # ============================================

    return cM


def calcAccuracyCM(cM):
    """Calculates prediction accuracy from a confusion matrix.

    Args:
        cM (array): Confusion matrix, with predicted labels in the rows
            and actual labels in the columns.

    Returns:
        acc (float): Prediction accuracy.
    """

    # --------------------------------------------
    # === Your code here =========================
    # --------------------------------------------
    acc = np.trace(cM) / np.sum(cM)
    # ============================================
    
    return acc
