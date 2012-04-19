package org.siemac.metamac.statistical.operations.web.server;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.domain.statistical.operations.enume.domain.StatisticalOperationsRoleEnum;
import org.siemac.metamac.sso.client.MetamacPrincipal;
import org.siemac.metamac.sso.client.MetamacPrincipalAccess;
import org.siemac.metamac.sso.client.SsoClientConstants;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConstants;

public class ServiceContextHelper {

    private static ServiceContext serviceContext;

    public static ServiceContext getServiceContext() {
        if (serviceContext == null) {

            serviceContext = new ServiceContext("user", "12345", StatisticalOperationsConstants.SECURITY_APPLICATION_ID);

            MetamacPrincipal metamacPrincipal = new MetamacPrincipal();
            metamacPrincipal.setUserId(serviceContext.getUserId());
            metamacPrincipal.getAccesses().add(new MetamacPrincipalAccess(StatisticalOperationsRoleEnum.ADMINISTRADOR.getName(), StatisticalOperationsConstants.SECURITY_APPLICATION_ID, null));
            serviceContext.setProperty(SsoClientConstants.PRINCIPAL_ATTRIBUTE, metamacPrincipal);

        }
        return serviceContext;
    }

}
