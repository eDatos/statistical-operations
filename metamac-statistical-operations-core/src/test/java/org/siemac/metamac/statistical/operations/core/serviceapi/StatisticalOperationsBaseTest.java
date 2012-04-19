package org.siemac.metamac.statistical.operations.core.serviceapi;

import java.util.List;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.common.test.MetamacBaseTests;
import org.siemac.metamac.domain.statistical.operations.enume.domain.StatisticalOperationsRoleEnum;
import org.siemac.metamac.sso.client.MetamacPrincipal;
import org.siemac.metamac.sso.client.MetamacPrincipalAccess;
import org.siemac.metamac.sso.client.SsoClientConstants;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConstants;

public abstract class StatisticalOperationsBaseTest extends MetamacBaseTests {

    // --------------------------------------------------------------------------------------------------------------
    // SERVICE CONTEXT
    // --------------------------------------------------------------------------------------------------------------

    @Override
    protected ServiceContext getServiceContext() {
        ServiceContext serviceContext = super.getServiceContext();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.ADMINISTRADOR);
        return serviceContext;
    }

    @Override
    protected ServiceContext getServiceContext2() {
        ServiceContext serviceContext = super.getServiceContext();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.ADMINISTRADOR);
        return serviceContext;
    }
    
    protected ServiceContext getServiceContextTecnicoProduccion() {
        ServiceContext serviceContext = super.getServiceContext();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.TECNICO_PRODUCCION);
        return serviceContext;
    }

    protected ServiceContext getServiceContextTecnicoApoyoProduccion() {
        ServiceContext serviceContext = super.getServiceContext();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.TECNICO_APOYO_PRODUCCION);
        return serviceContext;
    }

    protected ServiceContext getServiceContextTecnicoPlanificacion() {
        ServiceContext serviceContext = super.getServiceContext();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.TECNICO_PLANIFICACION);
        return serviceContext;
    }

    protected ServiceContext getServiceContextTecnicoApoyoPlanificacion() {
        ServiceContext serviceContext = super.getServiceContext();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.TECNICO_APOYO_PLANIFICACION);
        return serviceContext;
    }

    protected ServiceContext getServiceContextTecnicoDifusion() {
        ServiceContext serviceContext = super.getServiceContext();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.TECNICO_DIFUSION);
        return serviceContext;
    }

    protected ServiceContext getServiceContextTecnicoApoyoDifusion() {
        ServiceContext serviceContext = super.getServiceContext();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.TECNICO_APOYO_DIFUSION);
        return serviceContext;
    }

    private void putMetamacPrincipalInServiceContext(ServiceContext serviceContext, StatisticalOperationsRoleEnum role) {
        MetamacPrincipal metamacPrincipal = new MetamacPrincipal();
        metamacPrincipal.setUserId(serviceContext.getUserId());
        metamacPrincipal.getAccesses().add(new MetamacPrincipalAccess(role.getName(), StatisticalOperationsConstants.SECURITY_APPLICATION_ID, null));
        serviceContext.setProperty(SsoClientConstants.PRINCIPAL_ATTRIBUTE, metamacPrincipal);
    }

    // --------------------------------------------------------------------------------------------------------------
    // DBUNIT CONFIGURATION
    // --------------------------------------------------------------------------------------------------------------

    @Override
    protected String getDataSetFile() {
        return "dbunit/StatisticalOperationsBaseServiceTest.xml";
    }

    @Override
    protected List<String> getTablesToRemoveContent() {
        return null;
    }

    @Override
    protected List<String> getSequencesToRestart() {
        return null;
    }

    @Override
    public void tearDownDatabaseTester() throws Exception {
        // NOTHING;
    }
}
