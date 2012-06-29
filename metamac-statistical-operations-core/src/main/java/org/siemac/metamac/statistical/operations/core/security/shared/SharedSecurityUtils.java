package org.siemac.metamac.statistical.operations.core.security.shared;

import org.siemac.metamac.sso.client.MetamacPrincipal;
import org.siemac.metamac.sso.client.MetamacPrincipalAccess;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConstants;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatisticalOperationsRoleEnum;

public class SharedSecurityUtils {

    /**
     * Checks user has any role
     */
    public static boolean isUserInRol(MetamacPrincipal metamacPrincipal, StatisticalOperationsRoleEnum role) {
        if (StatisticalOperationsRoleEnum.ANY_ROLE_ALLOWED.equals(role)) {
            return isAnyStatisticalOperationsRole(metamacPrincipal);
        } else {
            return isRoleInAccesses(metamacPrincipal, role);
        }
    }

    /**
     * Checks if user has access to an operation. To have access, any access must exists to specified rol and operation, or has any access with
     * role and operation with 'null' value
     */
    public static boolean haveAccessToOperationInRol(MetamacPrincipal metamacPrincipal, StatisticalOperationsRoleEnum role, String operation) {
        for (MetamacPrincipalAccess metamacPrincipalAccess : metamacPrincipal.getAccesses()) {
            if (StatisticalOperationsConstants.SECURITY_APPLICATION_ID.equals(metamacPrincipalAccess.getApplication()) && metamacPrincipalAccess.getRole().equals(role.name())) {
                if (metamacPrincipalAccess.getOperation() == null || metamacPrincipalAccess.getOperation().equals(operation)) {
                    return Boolean.TRUE;
                }
            }
        }
        return Boolean.FALSE;
    }

    public static Boolean isAdministrador(MetamacPrincipal metamacPrincipal) {
        return isRoleInAccesses(metamacPrincipal, StatisticalOperationsRoleEnum.ADMINISTRADOR);
    }

    /**
     * Checks if user has access with role
     */
    public static Boolean isRoleInAccesses(MetamacPrincipal metamacPrincipal, StatisticalOperationsRoleEnum role) {
        for (MetamacPrincipalAccess metamacPrincipalAccess : metamacPrincipal.getAccesses()) {
            if (StatisticalOperationsConstants.SECURITY_APPLICATION_ID.equals(metamacPrincipalAccess.getApplication()) && metamacPrincipalAccess.getRole().equals(role.name())) {
                return Boolean.TRUE;
            }
        }
        return Boolean.FALSE;
    }

    public static Boolean isAnyStatisticalOperationsRole(MetamacPrincipal metamacPrincipal) {
        return isAdministrador(metamacPrincipal) || isTecnicoPlanificacion(metamacPrincipal) || isTecnicoApoyoPlanificacion(metamacPrincipal) || isTecnicoDifusion(metamacPrincipal)
                || isTecnicoApoyoDifusion(metamacPrincipal) || isTecnicoProduccion(metamacPrincipal) || isTecnicoApoyoProduccion(metamacPrincipal);
    }


    public static Boolean isTecnicoPlanificacion(MetamacPrincipal metamacPrincipal) {
        return isRoleInAccesses(metamacPrincipal, StatisticalOperationsRoleEnum.TECNICO_PLANIFICACION);
    }

    public static Boolean isTecnicoApoyoPlanificacion(MetamacPrincipal metamacPrincipal) {
        return isRoleInAccesses(metamacPrincipal, StatisticalOperationsRoleEnum.TECNICO_APOYO_PLANIFICACION);
    }

    public static Boolean isTecnicoDifusion(MetamacPrincipal metamacPrincipal) {
        return isRoleInAccesses(metamacPrincipal, StatisticalOperationsRoleEnum.TECNICO_DIFUSION);
    }

    public static Boolean isTecnicoApoyoDifusion(MetamacPrincipal metamacPrincipal) {
        return isRoleInAccesses(metamacPrincipal, StatisticalOperationsRoleEnum.TECNICO_APOYO_DIFUSION);
    }

    public static Boolean isTecnicoProduccion(MetamacPrincipal metamacPrincipal) {
        return isRoleInAccesses(metamacPrincipal, StatisticalOperationsRoleEnum.TECNICO_PRODUCCION);
    }

    public static Boolean isTecnicoApoyoProduccion(MetamacPrincipal metamacPrincipal) {
        return isRoleInAccesses(metamacPrincipal, StatisticalOperationsRoleEnum.TECNICO_APOYO_PRODUCCION);
    }

}
