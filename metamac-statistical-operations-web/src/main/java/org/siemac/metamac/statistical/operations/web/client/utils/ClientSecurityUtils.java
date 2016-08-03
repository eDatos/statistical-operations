package org.siemac.metamac.statistical.operations.web.client.utils;

import static org.siemac.metamac.statistical.operations.core.enume.domain.StatisticalOperationsRoleEnum.TECNICO_APOYO_PLANIFICACION;
import static org.siemac.metamac.statistical.operations.core.enume.domain.StatisticalOperationsRoleEnum.TECNICO_APOYO_PRODUCCION;
import static org.siemac.metamac.statistical.operations.core.enume.domain.StatisticalOperationsRoleEnum.TECNICO_PLANIFICACION;
import static org.siemac.metamac.statistical.operations.core.enume.domain.StatisticalOperationsRoleEnum.TECNICO_PRODUCCION;

import org.siemac.metamac.sso.client.MetamacPrincipal;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatisticalOperationsRoleEnum;
import org.siemac.metamac.statistical.operations.core.security.shared.SharedSecurityUtils;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;

public class ClientSecurityUtils {

    // FAMILIES

    public static boolean canCreateFamily() {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles)) {
            return true;
        }
        return false;
    }

    public static boolean canUpdateFamily() {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles)) {
            return true;
        }
        return false;
    }

    public static boolean canDeleteFamily() {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles)) {
            return true;
        }
        return false;
    }

    public static boolean canPublishFamilyInternally() {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles)) {
            return true;
        }
        return false;
    }

    public static boolean canPublishFamilyExternally() {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles)) {
            return true;
        }
        return false;
    }

    public static boolean canAddOperationToFamily() {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles)) {
            return true;
        }
        return false;
    }

    public static boolean canRemoveOperationFromFamily() {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles)) {
            return true;
        }
        return false;
    }

    // OPERATIONS

    public static boolean canCreateOperation() {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles)) {
            return true;
        }
        return false;
    }

    public static boolean canUpdateOperation(String operationCode) {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PRODUCCION, TECNICO_APOYO_PLANIFICACION, TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles) && isOperationAllowed(operationCode, roles)) {
            return true;
        }
        return false;
    }

    public static boolean canDeleteOperation(String operationCode, ProcStatusEnum procStatusEnum) {

        if (!ProcStatusEnum.DRAFT.equals(procStatusEnum)) {
            return false;
        }

        StatisticalOperationsRoleEnum[] roles = {TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles) && isOperationAllowed(operationCode, roles)) {
            return true;
        }
        return false;
    }

    public static boolean canPublishOperationInternally(String operationCode) {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles) && isOperationAllowed(operationCode, roles)) {
            return true;
        }
        return false;
    }

    public static boolean canPublishOperationExternally(String operationCode) {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles) && isOperationAllowed(operationCode, roles)) {
            return true;
        }
        return false;
    }

    public static boolean canAddFamilyToOperation(String operationCode) {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PRODUCCION, TECNICO_APOYO_PLANIFICACION, TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles) && isOperationAllowed(operationCode, roles)) {
            return true;
        }
        return false;
    }

    public static boolean canRemoveFamilyFromOperation(String operationCode) {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PRODUCCION, TECNICO_APOYO_PLANIFICACION, TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles) && isOperationAllowed(operationCode, roles)) {
            return true;
        }
        return false;
    }

    // INSTANCES

    public static boolean canCreateInstance(String operationCode) {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_APOYO_PRODUCCION, TECNICO_PRODUCCION, TECNICO_APOYO_PLANIFICACION, TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles) && isOperationAllowed(operationCode, roles)) {
            return true;
        }
        return false;
    }

    public static boolean canUpdateInstance(String operationCode) {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_APOYO_PRODUCCION, TECNICO_PRODUCCION, TECNICO_APOYO_PLANIFICACION, TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles) && isOperationAllowed(operationCode, roles)) {
            return true;
        }
        return false;
    }

    public static boolean canUpdateInstancesOrder(String operationCode) {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_APOYO_PRODUCCION, TECNICO_PRODUCCION, TECNICO_APOYO_PLANIFICACION, TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles) && isOperationAllowed(operationCode, roles)) {
            return true;
        }
        return false;
    }

    public static boolean canDeleteInstance(String operationCode, ProcStatusEnum procStatusEnum) {

        if (!ProcStatusEnum.DRAFT.equals(procStatusEnum)) {
            return false;
        }

        StatisticalOperationsRoleEnum[] roles = {TECNICO_PRODUCCION, TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles) && isOperationAllowed(operationCode, roles)) {
            return true;
        }
        return false;
    }

    public static boolean canPublishInstanceInternally(String operationCode) {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PRODUCCION, TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles) && isOperationAllowed(operationCode, roles)) {
            return true;
        }
        return false;
    }

    public static boolean canPublishInstanceExternally(String operationCode) {
        StatisticalOperationsRoleEnum[] roles = {TECNICO_PRODUCCION, TECNICO_PLANIFICACION};
        if (isRoleAllowed(roles) && isOperationAllowed(operationCode, roles)) {
            return true;
        }
        return false;
    }

    /**
     * Checks if logged user has one of the allowed roles
     * 
     * @param roles
     * @return
     */
    private static boolean isRoleAllowed(StatisticalOperationsRoleEnum... roles) {
        MetamacPrincipal userPrincipal = OperationsWeb.getCurrentUser();
        // Administration has total control
        if (SharedSecurityUtils.isAdministrador(userPrincipal)) {
            return true;
        }
        // Checks user has any role of requested
        if (roles != null) {
            for (int i = 0; i < roles.length; i++) {
                StatisticalOperationsRoleEnum role = roles[i];
                if (SharedSecurityUtils.isUserInRol(userPrincipal, role)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Checks if logged user has access to an operation with one of the selected roles
     * 
     * @param operationCode
     * @param roles
     * @return
     */
    private static boolean isOperationAllowed(String operationCode, StatisticalOperationsRoleEnum... roles) {
        MetamacPrincipal userPrincipal = OperationsWeb.getCurrentUser();
        // Administrator has total control in all operations
        if (SharedSecurityUtils.isAdministrador(userPrincipal)) {
            return true;
        }
        // Checks if operation is in some role
        if (roles != null) {
            for (int i = 0; i < roles.length; i++) {
                StatisticalOperationsRoleEnum role = roles[i];
                if (SharedSecurityUtils.haveAccessToOperationInRol(userPrincipal, role, operationCode)) {
                    return true;
                }
            }
        }
        return false;
    }

}
