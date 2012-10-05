package org.siemac.metamac.statistical.operations.core.serviceapi;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.common.test.MetamacBaseTests;
import org.siemac.metamac.sso.client.MetamacPrincipal;
import org.siemac.metamac.sso.client.MetamacPrincipalAccess;
import org.siemac.metamac.sso.client.SsoClientConstants;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConstants;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatisticalOperationsRoleEnum;

public abstract class StatisticalOperationsBaseTest extends MetamacBaseTests {

    public final String OPERATION_01 = "C0025A";
    public final String OPERATION_02 = "C0025B";

    // --------------------------------------------------------------------------------------------------------------
    // SERVICE CONTEXT
    // --------------------------------------------------------------------------------------------------------------

    @Override
    protected ServiceContext getServiceContextAdministrador() {
        ServiceContext serviceContext = super.getServiceContextWithoutPrincipal();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.ADMINISTRADOR);
        return serviceContext;
    }

    protected ServiceContext getServiceContextTecnicoProduccion() {
        ServiceContext serviceContext = super.getServiceContextWithoutPrincipal();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.TECNICO_PRODUCCION);
        return serviceContext;
    }

    protected ServiceContext getServiceContextTecnicoProduccionOperation01() {
        ServiceContext serviceContext = super.getServiceContextWithoutPrincipal();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.TECNICO_PRODUCCION, OPERATION_01);
        return serviceContext;
    }

    protected ServiceContext getServiceContextTecnicoProduccionOperation02() {
        ServiceContext serviceContext = super.getServiceContextWithoutPrincipal();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.TECNICO_PRODUCCION, OPERATION_02);
        return serviceContext;
    }

    protected ServiceContext getServiceContextTecnicoApoyoProduccion() {
        ServiceContext serviceContext = super.getServiceContextWithoutPrincipal();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.TECNICO_APOYO_PRODUCCION);
        return serviceContext;
    }
    protected ServiceContext getServiceContextTecnicoPlanificacion() {
        ServiceContext serviceContext = super.getServiceContextWithoutPrincipal();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.TECNICO_PLANIFICACION);
        return serviceContext;
    }

    protected ServiceContext getServiceContextTecnicoApoyoPlanificacion() {
        ServiceContext serviceContext = super.getServiceContextWithoutPrincipal();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.TECNICO_APOYO_PLANIFICACION);
        return serviceContext;
    }

    protected ServiceContext getServiceContextTecnicoDifusion() {
        ServiceContext serviceContext = super.getServiceContextWithoutPrincipal();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.TECNICO_DIFUSION);
        return serviceContext;
    }

    protected ServiceContext getServiceContextTecnicoApoyoDifusion() {
        ServiceContext serviceContext = super.getServiceContextWithoutPrincipal();
        putMetamacPrincipalInServiceContext(serviceContext, StatisticalOperationsRoleEnum.TECNICO_APOYO_DIFUSION);
        return serviceContext;
    }

    private void putMetamacPrincipalInServiceContext(ServiceContext serviceContext, StatisticalOperationsRoleEnum role) {
        MetamacPrincipal metamacPrincipal = new MetamacPrincipal();
        metamacPrincipal.setUserId(serviceContext.getUserId());
        metamacPrincipal.getAccesses().add(new MetamacPrincipalAccess(role.getName(), StatisticalOperationsConstants.SECURITY_APPLICATION_ID, null));
        serviceContext.setProperty(SsoClientConstants.PRINCIPAL_ATTRIBUTE, metamacPrincipal);
    }

    private void putMetamacPrincipalInServiceContext(ServiceContext serviceContext, StatisticalOperationsRoleEnum role, String operationCode) {
        MetamacPrincipal metamacPrincipal = new MetamacPrincipal();
        metamacPrincipal.setUserId(serviceContext.getUserId());
        metamacPrincipal.getAccesses().add(new MetamacPrincipalAccess(role.getName(), StatisticalOperationsConstants.SECURITY_APPLICATION_ID, operationCode));
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
    protected List<String> getTableNamesOrderedByFKDepedency() {
        List<String> tables = new ArrayList<String>();
        tables.add("TB_INTERNATIONAL_STRINGS");
        tables.add("TB_FAMILIES");

        tables.add("TB_EXTERNAL_ITEMS");

        tables.add("TB_OPERATIONS");
        tables.add("TB_FAMILIES_OPERATIONS");
        tables.add("TB_INSTANCES");
        tables.add("TB_INSTANCES_COSTS");

        tables.add("TB_EI_CLASS_SYSTEM_LISTS");
        tables.add("TB_EI_CONC_DEF_LISTS");
        tables.add("TB_EI_FREQ_COLL");
        tables.add("TB_EI_INF_SUPPLIERS");
        tables.add("TB_EI_PRODUCERS");
        tables.add("TB_EI_PUBLISHERS");
        tables.add("TB_EI_REG_CONTRIBUTORS");
        tables.add("TB_EI_REG_RESPONSIBLES");
        tables.add("TB_EI_SECONDARY_AREAS");
        tables.add("TB_EI_STATISTICAL_UNITS");
        tables.add("TB_EI_UNITS_MEASURE");
        tables.add("TB_EI_UPDATE_FREQUENCY");

        tables.add("TB_LOCALISED_STRINGS");
        return tables;
    }

    @Override
    protected List<String> getSequencesToRestart() {
        List<String> sequences = new ArrayList<String>();
        sequences.add("SEQ_I18NSTRS");
        sequences.add("SEQ_L10NSTRS");
        sequences.add("SEQ_EXTERNAL_ITEMS");
        sequences.add("SEQ_COMMON_METADATA");
        sequences.add("SEQ_FAMILIES");
        sequences.add("SEQ_OPERATIONS");
        sequences.add("SEQ_INSTANCES");
        sequences.add("SEQ_SURVEY_TYPES");
        sequences.add("SEQ_INSTANCE_TYPES");
        sequences.add("SEQ_ACTIVITY_CLASSES");
        sequences.add("SEQ_SURVEY_SOURCES");
        sequences.add("SEQ_OFFICIALITY_TYPES");
        sequences.add("SEQ_COLL_METHODS");
        sequences.add("SEQ_COSTS");
        return sequences;
    }

    @Override
    protected Map<String, String> getTablePrimaryKeys() {
        return null;
    }

}
