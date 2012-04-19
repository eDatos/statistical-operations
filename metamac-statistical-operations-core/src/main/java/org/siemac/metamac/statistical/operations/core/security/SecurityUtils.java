package org.siemac.metamac.statistical.operations.core.security;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.enume.domain.StatisticalOperationsRoleEnum;
import org.siemac.metamac.sso.client.MetamacPrincipal;
import org.siemac.metamac.sso.client.MetamacPrincipalAccess;
import org.siemac.metamac.sso.client.SsoClientConstants;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConstants;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;

public class SecurityUtils {

    /**
     * Checks user can execute any operation, if has any role of requested roles
     */
    public static void checkServiceOperationAllowed(ServiceContext ctx, StatisticalOperationsRoleEnum... roles) throws MetamacException {

        MetamacPrincipal metamacPrincipal = getMetamacPrincipal(ctx);

        // Administration has total control
        if (isAdministrador(metamacPrincipal)) {
            return;
        }
        // Checks user has any role of requested
        if (roles != null) {
            for (int i = 0; i < roles.length; i++) {
                StatisticalOperationsRoleEnum role = roles[i];
                if (isUserInRol(metamacPrincipal, role)) {
                    return;
                }
            }
        }
        throw new MetamacException(ServiceExceptionType.SECURITY_OPERATION_NOT_ALLOWED, metamacPrincipal.getUserId());
    }

    /**
     * Checks user has access to an operation or indicators system. To have access, user must have this indicators system in any role of requested roles
     */
    public static void checkResourceOperationAllowed(ServiceContext ctx, String operationCode, StatisticalOperationsRoleEnum... roles) throws MetamacException {

        MetamacPrincipal metamacPrincipal = getMetamacPrincipal(ctx);

        // Administration has total control in all indicators systems
        if (isAdministrador(metamacPrincipal)) {
            return;
        }
        
        // Checks if operation is in any role
        if (roles != null) {
            for (int i = 0; i < roles.length; i++) {
                StatisticalOperationsRoleEnum role = roles[i];
                if (haveAccessToOperationInRol(metamacPrincipal, role, operationCode)) {
                    return;
                }
            }
        }
        throw new MetamacException(ServiceExceptionType.SECURITY_ACCESS_OPERATION_NOT_ALLOWED, operationCode, metamacPrincipal.getUserId());
    }

    /**
     * Checks user has any rol
     */
    private static boolean isUserInRol(MetamacPrincipal metamacPrincipal, StatisticalOperationsRoleEnum role) throws MetamacException {

        switch (role) {
            case ADMINISTRADOR:
                return isAdministrador(metamacPrincipal);
            case ANY_ROLE_ALLOWED:
                return isAnyStatisticalOperationsRole(metamacPrincipal);
            case TECNICO_PLANIFICACION:
                return isTecnicoPlanificacion(metamacPrincipal);
            case TECNICO_APOYO_PLANIFICACION:
                return isTecnicoApoyoPlanificacion(metamacPrincipal);
            case TECNICO_DIFUSION:
                return isTecnicoDifusion(metamacPrincipal);
            case TECNICO_APOYO_DIFUSION:
                return isTecnicoApoyoDifusion(metamacPrincipal);
            case TECNICO_PRODUCCION:
                return isTecnicoProduccion(metamacPrincipal);
            case TECNICO_APOYO_PRODUCCION:
                return isTecnicoApoyoProduccion(metamacPrincipal);
            default:
                throw new MetamacException(ServiceExceptionType.UNKNOWN, "Operation not supported in security checker: " + role);
        }
    }

    /**
     * Checks if user has access to an operation. To have access, any access must exists to specified rol and operation, or has any access with
     * role and operation with 'null' value
     */
    private static boolean haveAccessToOperationInRol(MetamacPrincipal metamacPrincipal, StatisticalOperationsRoleEnum role, String operation) throws MetamacException {
        for (MetamacPrincipalAccess metamacPrincipalAccess : metamacPrincipal.getAccesses()) {
            if (StatisticalOperationsConstants.SECURITY_APPLICATION_ID.equals(metamacPrincipalAccess.getApplication()) && metamacPrincipalAccess.getRole().equals(role.name())) {
                if (metamacPrincipalAccess.getOperation() == null || metamacPrincipalAccess.getOperation().equals(operation)) {
                    return Boolean.TRUE;
                }
            }
        }
        return Boolean.FALSE;
    }

    /**
     * Retrieves MetamacPrincipal in ServiceContext
     */
    private static MetamacPrincipal getMetamacPrincipal(ServiceContext ctx) throws MetamacException {
        Object principalProperty = ctx.getProperty(SsoClientConstants.PRINCIPAL_ATTRIBUTE);
        if (principalProperty == null) {
            throw new MetamacException(ServiceExceptionType.SECURITY_PRINCIPAL_NOT_FOUND);
        }
        MetamacPrincipal metamacPrincipal = (MetamacPrincipal) principalProperty;
        if (!metamacPrincipal.getUserId().equals(ctx.getUserId())) {
            throw new MetamacException(ServiceExceptionType.SECURITY_PRINCIPAL_NOT_FOUND);
        }
        return metamacPrincipal;
    }

    /**
     * Checks if user has access with role
     */
    private static Boolean isRoleInAccesses(MetamacPrincipal metamacPrincipal, StatisticalOperationsRoleEnum role) {
        for (MetamacPrincipalAccess metamacPrincipalAccess : metamacPrincipal.getAccesses()) {
            if (StatisticalOperationsConstants.SECURITY_APPLICATION_ID.equals(metamacPrincipalAccess.getApplication()) && metamacPrincipalAccess.getRole().equals(role.name())) {
                return Boolean.TRUE;
            }
        }
        return Boolean.FALSE;
    }

    private static Boolean isAnyStatisticalOperationsRole(MetamacPrincipal metamacPrincipal) {
        return isAdministrador(metamacPrincipal) || isTecnicoPlanificacion(metamacPrincipal) || isTecnicoApoyoPlanificacion(metamacPrincipal) || isTecnicoDifusion(metamacPrincipal)
                || isTecnicoApoyoDifusion(metamacPrincipal) || isTecnicoProduccion(metamacPrincipal) || isTecnicoApoyoProduccion(metamacPrincipal);
    }

    private static Boolean isAdministrador(MetamacPrincipal metamacPrincipal) {
        return isRoleInAccesses(metamacPrincipal, StatisticalOperationsRoleEnum.ADMINISTRADOR);
    }

    private static Boolean isTecnicoPlanificacion(MetamacPrincipal metamacPrincipal) {
        return isRoleInAccesses(metamacPrincipal, StatisticalOperationsRoleEnum.TECNICO_PLANIFICACION);
    }

    private static Boolean isTecnicoApoyoPlanificacion(MetamacPrincipal metamacPrincipal) {
        return isRoleInAccesses(metamacPrincipal, StatisticalOperationsRoleEnum.TECNICO_APOYO_PLANIFICACION);
    }

    private static Boolean isTecnicoDifusion(MetamacPrincipal metamacPrincipal) {
        return isRoleInAccesses(metamacPrincipal, StatisticalOperationsRoleEnum.TECNICO_DIFUSION);
    }

    private static Boolean isTecnicoApoyoDifusion(MetamacPrincipal metamacPrincipal) {
        return isRoleInAccesses(metamacPrincipal, StatisticalOperationsRoleEnum.TECNICO_APOYO_DIFUSION);
    }

    private static Boolean isTecnicoProduccion(MetamacPrincipal metamacPrincipal) {
        return isRoleInAccesses(metamacPrincipal, StatisticalOperationsRoleEnum.TECNICO_PRODUCCION);
    }

    private static Boolean isTecnicoApoyoProduccion(MetamacPrincipal metamacPrincipal) {
        return isRoleInAccesses(metamacPrincipal, StatisticalOperationsRoleEnum.TECNICO_APOYO_PRODUCCION);
    }
}